open Colis_batch_ext
module Model = Colis_batch_model

type t =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    package : Model.Package.t ;
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran) list }
[@@deriving yojson { exn = true }]

let make ~meta ~config package scenarii =
  { meta ; config ; package ; scenarii }

let save_as_bin ~cache (report : t) =
  let path = Filename.concat cache (spf "%x.bin" (Hashtbl.hash report)) in
  Path.with_lock_on path @@ fun () ->
  let oc = open_out path in
  Marshal.to_channel oc report [];
  close_out oc

let load_as_bin ~cache file : t =
  let path = Filename.concat cache file in
  Path.with_lock_on path @@ fun () ->
  let ic = open_in path in
  let v = Marshal.from_channel ic in
  close_in ic;
  v

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
