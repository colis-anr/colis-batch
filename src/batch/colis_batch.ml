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

let handle_package path =
  epf "Handling package at %s.@." path;
  let package = Colis_package.parse_package path in
  let scenarii =
    Colis_package.map_all_scenarii
      (fun (name, scenario) ->
         try
           let ran = Colis_package.run_scenario
               ~cpu_timeout:!Colis_config.cpu_timeout
               ~package scenario in
           Some (name, ran)
         with
           Invalid_argument _ -> None)
    |> List.map_filter Fun.id
  in
  Colis_package.generate_and_write_html_report
    ~prefix:(Filename.concat_l [!Colis_config.report; "package"; Colis_package.Package.name package])
    package scenarii;
  (package, scenarii)

let () =
  let paths = !Colis_config.corpus |> Sys.readdir |> Array.to_list |> List.map (Filename.concat !Colis_config.corpus) in
  let packages =
    MultiProcess.map_p ~workers:!Colis_config.workers handle_package paths
    |> Lwt_main.run
  in
  List.iter
    (fun (package, _) -> pf "- %s@." (Colis_package.Package.name package))
    packages
