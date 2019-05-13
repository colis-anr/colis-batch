let () =
  (
    try
      Options.parse_command_line ();
      Options.check_values ()
    with
      Arg.Bad msg ->
      print_endline msg;
      print_newline ();
      Options.print_usage ();
      exit 1
  );
  (* FIXME: The following is super dirty and should be fixed in Colis-Language
    where the CPU timeout should be an option. And it's actually buggy because
    this will only consider the time from the beginning of all the execution. *)
  Constraints_common.Log.cpu_time_limit := Some (!Options.cpu_timeout);
  Colis.Options.external_sources := !Options.external_sources

let find_packages () =
  !Options.corpus
  |> Sys.readdir
  |> Array.to_list

let pf = Format.printf

let () =
  let packages = find_packages () in
  pf "Found %d packages.@." (List.length packages);
  let parsed_packages = ExtList.map_filter Package.parse packages in
  pf "Parsed %d packages successfully.@." (List.length parsed_packages);
  List.iter
    (fun package ->
       let _scenario = Scenarii.(run ~package Install) in
       pf "Package: %s@."
         (Package.name package))
    parsed_packages;
  pf "Generating report.@.";
  HtmlReport.generate_and_write ()
