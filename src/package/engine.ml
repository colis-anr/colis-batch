open Colis_ext

let handle_package path =
  HtmlReport.with_formatter_to_report ~title:"Package Report" ~viz:true ["index.html"] @@ fun fmt ->

  let package = Package.parse path in
  CliReport.pp_package_parsing_status package;
  HtmlReport.pp_package_parsing_status fmt package;

  Scenarii.all
  |> List.iter
    (fun (name, scenario) ->
       pf "Scenario: %s.@." (Scenario.name_to_string name);
       let ran = ScenarioEngine.run ~package ~name scenario in
       ignore ran);
  pf "@."
