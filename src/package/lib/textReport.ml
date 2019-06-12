open Colis_ext

let pp_parsing_status fmt package =
  fpf fmt "Parsed:@\n- name: %s@\n- version: %s@\n- scripts:@."
    (Package.name package) (Package.version package);
  Package.iter_maintscripts
    (fun (key, maintscript) ->
       fpf fmt "  - %s: %s@."
         (Maintscript.Key.to_string key)
         (if Maintscript.is_present maintscript then
            (match Maintscript.has_error maintscript with
             | None -> "OK"
             | Some e -> Maintscript.error_to_string e)
          else
            "absent"))
    package;
  fpf fmt "@."

let pp_scenario _package name fmt ran =
  fpf fmt "%s:@\n" (Scenario.name_to_string name);

  let rec pp_scenario fmt sc =
    match sc.Scenario.scenario with
    | Status st ->
      fpf fmt "%a@\n[%d states]"
        Scenario.Status.pp st
        (List.length sc.Scenario.data.Scenario.states)
    | Action (a, sc1, sc2) ->
      fpf fmt "%a@\n- @[%a@]@\n- @[%a@]"
        Scenario.pp_action a
        pp_scenario sc1
        pp_scenario sc2
  in
  fpf fmt "  @[%a@]@\n@." pp_scenario ran
