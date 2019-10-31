module Model = Colis_batch_model

type t =
  { start_time : float ;
    end_time : float ;
    package : Model.Package.t ;
    scenarii : (Model.Scenarii.Name.t * Model.Scenario.ran) list }

let make ~start_time ~end_time package scenarii =
  { start_time ; end_time ; package ; scenarii }
