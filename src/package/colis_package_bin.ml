open Colis_ext
module Config = Colis_common.Config

let one_package = true

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
  Constraints_common.Log.cpu_time_limit := Some (!Config.cpu_timeout);
  Colis.Options.external_sources := !Config.external_sources;
  Colis.Options.fail_on_unknown_utilities := true

(* FIXME: contents table *)
(* let () = ContentsTable.load () *)

let () =
  let path = unwrap !Config.package in
  let start_time = Unix.gettimeofday () in
  let package = Colis_package.parse_package path in
  let scenarii =
    Colis_package.map_all_scenarii
      (fun (name, scenario) ->
         let ran = Colis_package.run_scenario
             ~cpu_timeout:!Config.cpu_timeout
             ~package scenario in
         (name, ran))
  in
  let end_time = Unix.gettimeofday () in
  Colis_package.generate_and_write_html_report ~start_time ~end_time ~copy_static:true package scenarii
