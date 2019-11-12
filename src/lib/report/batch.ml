module Model = Colis_batch_model

type t =
  { start_time : float ;
    end_time : float ;
    packages : Package.summary list }
[@@deriving yojson { exn = true }]

let make ~start_time ~end_time packages =
  { start_time ; end_time ; packages }

let save_as_json ~prefix report =
  report
  |> to_yojson
  |> Yojson.Safe.to_file (Path.main_json_file ~prefix)

let load_as_json ~prefix =
  Yojson.Safe.from_file (Path.main_json_file ~prefix)
  |> of_yojson_exn
