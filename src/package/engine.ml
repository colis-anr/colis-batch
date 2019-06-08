open Colis_ext

let handle_package name =
  let report_path = ["package"; name; "index.html"] in (* FIXME: enlever le préfixe *)
  Report.with_formatter_to_report ~viz:true report_path @@ fun fmt ->
  pf "Package: %s.@." name;
  let package = Package.parse name in
  pf "Parsed (there may be errors in scripts).@.";
  Report.Package.pp_parsing_status fmt name;
  Scenarii.all
  |> List.iter
    (fun (name, scenario) ->
       pf "Scenario: %s.@." (Scenario.name_to_string name);
       let ran = ScenarioEngine.run ~package ~name scenario in
       ignore ran);
  pf "@."
