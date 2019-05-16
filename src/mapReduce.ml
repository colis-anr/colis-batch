open Lwt

type 'a message =
  | WantInput
  | Output of 'a

let map_reduce_p ~workers ~map ~reduce ~default tasks =
  let input_stream task_ichan req_ochan =
    Lwt_stream.from
      (fun () ->
         Lwt_io.write_value req_ochan WantInput >>= fun () ->
         try%lwt
           Lwt.return (Some (Lwt_io.read_value task_ichan))
         with
           End_of_file -> Lwt.return None)
  in
  let forkee task_ichan req_ochan =
    Lwt_stream.fold
      (fun x y -> reduce (map x) y)
      (input_stream task_ichan req_ochan)
      default
    >>= fun y ->
    Lwt_io.write_value req_ochan (Output y)
  in
  let tasks = ref tasks in
  let rep = ref default in
  let rec forker task_ochan req_ichan =
    match%lwt Lwt_io.read_value req_ichan with
    | WantInput ->
      (
        match !tasks with
        | [] -> Lwt_io.close task_ochan
        | task :: tasks' ->
          tasks := tasks';
          Lwt_io.write_value task_ochan task >>= fun () ->
          forker task_ochan req_ichan
      )
    | Output y ->
      rep := reduce !rep y;
      Lwt.return ()
  in
  Lwt_io.flush_all () >>= fun () ->
  List.init
    workers
    (fun _ ->
       let (task_ichan, task_ochan) = Lwt_io.pipe () in
       let (req_ichan, req_ochan) = Lwt_io.pipe () in
       match Lwt_unix.fork () with
       | 0 ->
         Lwt_main.run (forkee task_ichan req_ochan);
         exit 0
       | _pid ->
         forker task_ochan req_ichan)
  |> Lwt.join >>= fun () ->
  assert (!tasks = []);
  Lwt.return !rep
