open Colis_ext
module Config = Colis_common.Config

let one_package = false

let () =
  (
    try
      Config.parse_command_line ~one_package;
      Config.check_values ~one_package
    with
      Arg.Bad msg ->
      print_endline msg;
      print_newline ();
      Config.print_usage ~one_package;
      exit 1
  );
  (* FIXME: The following is super dirty and should be fixed in Colis-Language
     where the CPU timeout should be an option. And it's actually buggy because
     this will only consider the time from the beginning of all the execution. *)
  Colis.Internals.Options.cpu_time_limit := !Config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit !Config.memory_limit;
  Colis.Internals.Options.external_sources := !Config.external_sources;
  Colis.Internals.Options.fail_on_unknown_utilities := true

let () =
  epf "Parsing contents table... @?";
  List.iter Contents.scan !Colis_common.Config.contents;
  epf "done!@."

let handle_package path =
  epf "Handling package at %s.@." path;
  let start_time = Unix.gettimeofday () in
  let package = Colis_package.parse_package path in
  let scenarii =
    Colis_package.map_all_scenarii
      (fun (name, scenario) ->
         try
           let ran = Colis_package.run_scenario
               ~cpu_timeout:!Config.cpu_timeout
               ~package scenario in
           Some (name, ran)
         with
           Invalid_argument _ -> None)
    |> List.map_filter Fun.id
  in
  let end_time = Unix.gettimeofday () in
  Colis_package.generate_and_write_html_report
    ~start_time ~end_time
    ~prefix:[Colis_package.Package.name package, ["package"; Colis_package.Package.safe_name package]]
    ~copy_static:false
    package scenarii;
  (* We don't need the scenario nor the states for the general report, so we
     only remember the number of states per status. This will save lots of
     memory. *)
  let scenarii =
    List.map
      (fun (name, scenario) ->
         let scenario =
           List.map
             (fun (status, states) ->
                (status, List.length states))
             (Colis_package.Scenario.states scenario)
         in
         (name, scenario))
      scenarii
  in
  (package, scenarii)

let () =
  let start_time = Unix.gettimeofday () in
  let packages_and_scenarii =
    !Config.corpus
    |> Sys.readdir |> Array.to_list
    |> List.sort compare
    |> List.map (Filename.concat !Config.corpus)
    |> MultiProcess.map_p ~workers:!Config.workers handle_package
    |> Lwt_main.run
  in
  let end_time = Unix.gettimeofday () in
  HtmlReport.generate_and_write
    ~start_time ~end_time
    packages_and_scenarii
