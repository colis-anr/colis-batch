open Colis_ext

let handle_package path =
  pf "Package path: %s.@." path;

  let package = Package.parse path in
  pf "Parsed:@\n- name: %s@\n- version: %s@\n- scripts:@."
    (Package.name package) (Package.version package);
  Package.iter_maintscripts
    (fun (key, maintscript) ->
       pf "  - %s: %s@."
         (Maintscript.Key.to_string key)
         (if Maintscript.is_present maintscript then
            (match Maintscript.has_error maintscript with
             | None -> "OK"
             | Some e -> Maintscript.error_to_string e)
          else
            "absent"))
    package;

  Scenarii.all
  |> List.iter
    (fun (name, scenario) ->
       pf "Scenario: %s.@." (Scenario.name_to_string name);
       let ran = ScenarioEngine.run ~package ~name scenario in
       ignore ran);
  pf "@."
