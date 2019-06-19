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
        let input = input_value inputs_ichan in
        let output = try Ok (f input) with exn -> Error exn in
        output_value outputs_ochan output;
        flush outputs_ochan;
      done
    with
      _exn -> ()
  );
  exit 0

let forker inputs_ochan outputs_ichan q a =
  try%lwt
    while%lwt true do
      let (input, output_index) = Queue.take q in
      let%lwt () = Lwt_io.write_value inputs_ochan input in
      let%lwt () = Lwt_io.flush inputs_ochan in
      let%lwt output = Lwt_io.read_value outputs_ichan in
      if a.(output_index) <> None then
        raise Error;
      match output with
      | Ok output ->
        a.(output_index) <- Some output;
        Lwt.return ()
      | Error exn ->
        Lwt.fail exn
    done
  with
  | Queue.Empty -> Lwt.return ()

let map_p ~workers f l =
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
                   _exn ->
                   Lwt.return ())
          in
          create_workers q a (worker :: acc) (wid + 1)
      )
  in
  let (_ichan, _ochan) = Lwt_io.pipe () in
  let q = Queue.create () in
  let a = Array.make (List.length l) None in
  List.iteri (fun i x -> Queue.add (x, i) q) l;
  let%lwt () = create_workers q a [] 0 |> List.rev |> lwt_join_or_first_error in
  Array.fold_right
    (fun b l ->
       match b with
       | None -> raise Error
       | Some output -> output :: l)
    a
    []
  |> Lwt.return
