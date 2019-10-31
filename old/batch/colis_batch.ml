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
