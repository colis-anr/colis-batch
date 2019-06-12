open Colis_ext

let one_package = false

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

let () =
  epf "Parsing contents table... @?";
  ContentsTable.load ();
  epf "done!@."

let () =
  !Colis_config.corpus |> Sys.readdir |> Array.to_list
  |> MultiProcess.map_p
    ~workers:!Colis_config.workers
    (fun path ->
       epf "Handling package at %s.@." path;
       let path = Filename.concat !Colis_config.corpus path in
       let package = Colis_package.parse_package path in
       Colis_package.Package.iter_maintscripts
         (fun (_key, maintscript) ->
            if not (Colis_package.Maintscript.is_present maintscript)
            || Colis_package.Maintscript.has_error maintscript <> None then
              assert false)
         package;
       let scenarii =
         Colis_package.map_all_scenarii
           (fun (name, scenario) ->
              let ran = Colis_package.run_scenario
                  ~cpu_timeout:!Colis_config.cpu_timeout
                  ~package scenario in
              (name, ran))
       in
       Colis_package.generate_and_write_html_report
         ~prefix:(Filename.concat_l [!Colis_config.report; "package"; Colis_package.Package.name package])
         package scenarii)
  |> Lwt_main.run
  |> List.iter
    (fun package ->
       pf "- %s@." (Colis_package.Package.name package))
