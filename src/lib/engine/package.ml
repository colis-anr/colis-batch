open Colis_batch_ext
module Model = Colis_batch_model
module Report = Colis_batch_report

let analyse ~config package =
  let (meta, scenarii) =
    Report.Meta.while_gathering_meta @@ fun () ->
    Colis.Internals.Options.with_package_name (Model.Package.name package) @@ fun () ->
    Colis.Internals.Options.with_contents (Model.Package.content package) @@ fun () ->
    List.map
      (fun (name, scenario) ->
         try
           let ran = Scenario.run
               ~cpu_timeout:config.Colis_batch_config.cpu_timeout
               ~package scenario in
           Some (name, ran)
         with
           Invalid_argument _ -> None)
      Model.Scenarii.all
    |> List.map_filter Fun.id
  in
  Report.Package.make ~meta ~config package scenarii
