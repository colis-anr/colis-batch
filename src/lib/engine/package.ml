open Colis_batch_ext
module Model = Colis_batch_model
module Report = Colis_batch_report

let analyse package =
  let start_time = Unix.gettimeofday () in
  let scenarii =
    List.map
      (fun (name, scenario) ->
         try
           let ran = Scenario.run
               ~cpu_timeout:60. (*!Config.cpu_timeout (* FIXME *)*)
               ~package scenario in
           Some (name, ran)
         with
           Invalid_argument _ -> None)
      Model.Scenarii.all
    |> List.map_filter Fun.id
  in
  let end_time = Unix.gettimeofday () in
  Report.Package.make
    ~start_time ~end_time
    package scenarii
