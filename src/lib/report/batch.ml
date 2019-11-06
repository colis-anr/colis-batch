module Model = Colis_batch_model

type t =
  { start_time : float ;
    end_time : float ;
    packages : Package.summary list }
[@@deriving yojson { exn = true }]

let make _ = assert false

let report_file ~prefix = Filename.concat prefix "report.json"

let save ~prefix report =
  report
  |> to_yojson
  |> Yojson.Safe.to_file (report_file ~prefix)

let load ~prefix =
  Yojson.Safe.from_file (report_file ~prefix)
  |> of_yojson_exn
