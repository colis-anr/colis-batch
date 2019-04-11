
let find_packages () =
  (* FIXME. For now, we are doing it that way. But this will change. *)
  let packages = ref [] in
  let add_package package =
    packages := package :: !packages
  in
  let rec find_packages prefix =
    Sys.readdir prefix
    |> Array.iter
      (fun fname ->
         let path = Filename.concat prefix fname in
         if Sys.is_directory path then
           find_packages path
         else if List.mem fname ["preinst"; "postinst"; "prerm"; "postrm"] then
           add_package prefix)
  in
  find_packages !Options.corpus;
  List.sort_uniq compare !packages

let split_packages wid =
  let rec split_packages n = function
    | [] -> []
    | h :: q when n mod !Options.workers = wid ->
      h :: split_packages (n+1) q
    | _ :: q ->
      split_packages (n+1) q
  in
  split_packages 0

let parse_if_exists package file =
  let path = Filename.concat package file in
  if Sys.file_exists path then
    (
      try
        let colis = Colis.parse_shell_file path in
        Stats.(set_parsing_status package file Accepted);
        Some colis
      with
        exn ->
        Stats.(
          set_parsing_status
            package file
            (match exn with
             | Colis.Errors.ParseError (msg, _) -> MorbigRejected msg
             | Colis.Errors.ConversionError msg -> ConversionRejected msg
             | _ -> Unexpected (Printexc.to_string exn))
        );
        raise exn
    )
  else
    None

type package = {
  name : string ;
  scripts : (string * Colis.colis option) list ;
}

let parse_package package =
  try
    Some {
      name = package ;
      scripts = List.map
          (fun script -> (script, parse_if_exists package script))
          Constant.scripts ;
    }
  with
    _ -> None

let symbexec_package package =
  List.iter
    (fun script_name ->
       match List.assoc script_name package.scripts with
       | None -> ()
       | Some script ->
         Colis.run_symbolic
           Options.symbolic_config
           Colis.Symbolic.FilesystemSpec.empty (* FIXME *)
           ~argument0:script_name
           ~arguments:["install"] (* FIXME *)
           ~vars:[
             "DPKG_MAINTSCRIPT_NAME", script_name;
             "DPKG_MAINTSCRIPT_PACKAGE", package.name;
           ]
           script)
    Constant.scripts

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

let () =
  find_packages ()
  |> List.map parse_package
  |> List.iter (function
      | None -> ()
      | Some package -> symbexec_package package
    )
