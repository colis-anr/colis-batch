module Model = Colis_batch_model

type t =
  { start_time : float ;
    end_time : float ;
    package : Model.Package.t ;
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran) list }
[@@deriving yojson { exn = true }]

let make ~start_time ~end_time package scenarii =
  { start_time ; end_time ; package ; scenarii }

let save ~prefix report = ignore prefix; ignore report; assert false
let load ~prefix = ignore prefix; assert false

(* Summary version *)

type summary =
  { start_time : float ;
    end_time : float ;
    package : Model.Package.t ; (* FIXME: Summarize packages? *)
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran_sum) list }
[@@deriving yojson { exn = true }]

let summarize (full : t) =
  { start_time = full.start_time ;
    end_time = full.end_time ;
    package = full.package ;
    scenarii = List.map (fun (name, ran) -> (name, Model.Scenario.summarize ran)) full.scenarii }

let save_summary ~prefix report = ignore prefix; ignore report; assert false
let load_summary ~prefix = ignore prefix; assert false
