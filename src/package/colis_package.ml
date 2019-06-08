open Colis_ext

let one_package = true

let () =
  (
    try
      Colis_config.parse_command_line ~one_package;
      Colis_config.check_values ~one_package
    with
      Arg.Bad msg ->
      print_endline msg;
      print_newline ();
      Colis_config.print_usage ~one_package;
      exit 1
  );
  (* FIXME: The following is super dirty and should be fixed in Colis-Language
     where the CPU timeout should be an option. And it's actually buggy because
     this will only consider the time from the beginning of all the execution. *)
  Constraints_common.Log.cpu_time_limit := Some (!Colis_config.cpu_timeout);
  Colis.Options.external_sources := !Colis_config.external_sources

(* let () = ContentsTable.load () *)

let handle_package name =
  let report_path = ["package"; name; "index.html"] in (* FIXME: enlever le préfixe *)
  Report.with_formatter_to_report ~viz:true report_path @@ fun fmt ->
  pf "Package: %s.@." name;
  let package = Package.parse name in
  Report.Package.pp_parsing_status fmt name;
  (
    match package with
    | None ->
      pf "Parsing failed.@."
    | Some package ->
      pf "Parsing successful.@.";
      List.iter
        (fun (name, scenario) ->
           pf "Scenario: %s.@." (Scenario.name_to_string name);
           let ran = ScenarioEngine.run ~package ~name scenario in
           ignore ran) (* FIXME *)
        Scenarii.all;
      Report.Package.pp_scenarii fmt (Package.name package)
  );
  pf "@."


let () =
  handle_package (unwrap !Colis_config.package);
  Report.generate_and_write ()
