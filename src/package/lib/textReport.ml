open Colis_ext

let pp_parsing_status fmt package =
  fpf fmt "Parsed:@\n- name: %s@\n- version: %s@\n- scripts:@."
    (Package.name package) (Package.version package);
  Package.iter_maintscripts
    (fun maintscript ->
       fpf fmt "  - %s: %s@."
         (Maintscript.key_as_string maintscript)
         (match Maintscript.error maintscript with
          | None -> "OK"
          | Some e -> Maintscript.error_to_string e))
    package;
  fpf fmt "@."

let pp_scenario _package name fmt ran =
  fpf fmt "%s:@\n" (Scenario.name_to_string name);

  let rec pp_scenario fmt = function
    | Scenario.Status (states, status) ->
      fpf fmt "%a@\n[%d states]"
        Scenario.Status.pp status
        (List.length states)
    | RunScript (_ran_node, script, sc1, sc2) ->
      fpf fmt "%a@\n- @[%a@]@\n- @[%a@]"
        Scenario.pp_run_script script
        pp_scenario sc1
        pp_scenario sc2
  in
  fpf fmt "  @[%a@]@\n@." pp_scenario ran
