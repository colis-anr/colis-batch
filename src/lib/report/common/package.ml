module Model = Colis_batch_model

type t =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    package : Model.Package.t ;
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran) list }
[@@deriving yojson { exn = true }]

let make ~meta ~config package scenarii =
  { meta ; config ; package ; scenarii }

let save_as_json ~prefix report =
  let path =
    Path.package_json_file ~prefix
      ~package:(Model.Package.safe_name report.package)
  in
  report
  |> to_yojson
  |> Yojson.Safe.to_file path

let load_as_json ~prefix ~package =
  let path = Path.package_json_file ~prefix ~package in
  Yojson.Safe.from_file path
  |> of_yojson_exn

(* Summary version *)

type summary =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    package : Model.Package.t ; (* FIXME: Summarize packages? *)
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran_sum) list }
[@@deriving yojson { exn = true }]

let summarize (full : t) =
  { meta = full.meta ;
    config = full.config ;
    package = full.package ;
    scenarii = List.map (fun (name, ran) -> (name, Model.Scenario.summarize ran)) full.scenarii }

let save_summary_as_json ~prefix report =
  let path =
    Path.package_json_file ~prefix
      ~package:(Model.Package.safe_name report.package)
  in
  report
  |> summary_to_yojson
  |> Yojson.Safe.to_file path

let load_summary_as_json ~prefix ~package =
  let path = Path.package_json_file ~prefix ~package in
  Yojson.Safe.from_file path
  |> summary_of_yojson_exn

let save_summary_as_bin ~prefix (report : summary) =
  let path =
    Path.package_bin_file ~prefix
      ~package:(Model.Package.safe_name report.package)
  in
  let oc = open_out path in
  Marshal.to_channel oc report []

let load_summary_as_bin ~prefix ~package : summary =
  let path = Path.package_bin_file ~prefix ~package in
  let ic = open_in path in
  let v = Marshal.from_channel ic in
  close_in ic;
  v
