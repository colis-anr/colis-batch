exception Error

let lwt_join_or_first_error all =
  let rec lwt_join_or_first_error all =
    let%lwt (_, pending) = Lwt.nchoose_split all in
    if pending = [] then
      Lwt.return ()
    else
      lwt_join_or_first_error pending
  in
  try%lwt lwt_join_or_first_error all
  with exn -> List.iter Lwt.cancel all; Lwt.fail exn

let forkee inputs_ichan outputs_ochan f =
  (
    try
      while true do
        let (index, input) = input_value inputs_ichan in
        let output = try Ok (f index input) with exn -> Error exn in
        output_value outputs_ochan output;
        flush outputs_ochan;
      done
    with
      exn ->
      Format.eprintf
        "Unexpected exception in forkee:@\n%s@\n%s@."
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
      raise exn
  );
  exit 0

let forker inputs_ochan outputs_ichan q a =
  try%lwt
    while%lwt true do
      let (index, input) = Queue.take q in
      let%lwt () = Lwt_io.write_value inputs_ochan (index, input) in
      let%lwt () = Lwt_io.flush inputs_ochan in
      let%lwt output = Lwt_io.read_value outputs_ichan in
      if a.(index) <> None then
        raise Error;
      match output with
      | Ok output ->
        a.(index) <- Some output;
        Lwt.return ()
      | Error exn ->
        Lwt.fail exn
    done
  with
  | Queue.Empty -> Lwt.return ()

let mapi_p ~workers f l =
  let rec create_workers q a acc wid =
    if wid >= workers then
      acc
    else
      (
        let (inputs_ichan, inputs_ochan) =
          let (fd_in, fd_out) = Unix.pipe () in
          (Unix.in_channel_of_descr fd_in,
           Lwt_io.(of_unix_fd ~close:(fun () -> Unix.close fd_out; Lwt.return ()) ~mode:output fd_out))
        in
        let (outputs_ichan, outputs_ochan) =
          let (fd_in, fd_out) = Unix.pipe () in
          (Lwt_io.(of_unix_fd ~close:(fun () -> Unix.close fd_in; Lwt.return ()) ~mode:input fd_in),
           Unix.out_channel_of_descr fd_out)
        in
        match Lwt_unix.fork () with
        | 0 ->
          [forkee inputs_ichan outputs_ochan f] (* Not acc! *)
        | pid ->
          let worker =
            Lwt.finalize
              (fun () -> forker inputs_ochan outputs_ichan q a)
              (fun () ->
                 Unix.kill pid Sys.sigkill;
                 try%lwt
                   let%lwt () = Lwt_io.abort inputs_ochan in
                   close_in inputs_ichan;
                   close_out outputs_ochan;
                   let%lwt () = Lwt_io.abort outputs_ichan in
                   Lwt.return ()
                 with
                   exn ->
                   Format.eprintf "Unexpected exception in forker's wrapper:@\n%s@\n%s"
                     (Printexc.to_string exn)
                     (Printexc.get_backtrace ());
                   Lwt.return ())
          in
          create_workers q a (worker :: acc) (wid + 1)
      )
  in

  let (_ichan, _ochan) = Lwt_io.pipe () in (* Probably here to find errors earlier? *)
  let q = Queue.create () in
  let a = Array.make (List.length l) None in
  List.iteri (fun i x -> Queue.add (i, x) q) l;
  let%lwt () = create_workers q a [] 0 |> List.rev |> lwt_join_or_first_error in
  Array.fold_right
    (fun b l ->
       match b with
       | None -> raise Error
       | Some output -> output :: l)
    a
    []
  |> Lwt.return

let iteri_p ~workers f l =
  let%lwt _ = mapi_p ~workers f l in
  Lwt.return_unit

let map_p ~workers f l =
  mapi_p ~workers (fun _ -> f) l

let iter_p ~workers f l =
  iteri_p ~workers (fun _ -> f) l
