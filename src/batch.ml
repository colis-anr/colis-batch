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

let () =
  find_packages ()
  |> List.iter
    (fun package ->
       ignore (Package.parse ~name:package))
