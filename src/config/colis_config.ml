let spf = Format.sprintf

let contents = ref "contents"
let corpus = ref "corpus"
let cpu_timeout = ref 5.
let external_sources = ref "external_sources"
let package = ref None
let report = ref "report"
let workers = ref 2

let speclist ~one_package =
  let speclist =
    Arg.[
      "--contents",    Set_string contents,    spf "FILE Sets the path to the contents file (default: %s)" !contents;
      "--cpu-timeout", Set_float  cpu_timeout, spf "NB Sets the CPU timeout, in seconds (default: %.0f)" !cpu_timeout;
      "--external-sources", Set_string external_sources, spf "DIR Sets the path to the external sources (default: %s)" !external_sources;
      "--report",      Set_string report,      spf "DIR Sets the path to the report (default: %s)" !report;
      "--workers",     Set_int    workers,     spf "NB Sets the number of workers (default: %d)" !workers;
    ]
  in
  let speclist =
    if one_package then
      speclist @
      ["--corpus",      Set_string corpus,      spf "DIR Sets the path to the corpus (default: %s)" !corpus;]
    else
      speclist
  in
  speclist
  |> List.sort compare
  |> Arg.align

let usage ~one_package =
  spf "%s [OPTIONS]%s" Sys.argv.(0) (if one_package then " PACKAGE" else "")

let parse_command_line ~one_package =
  Arg.parse
    (speclist ~one_package)
    (fun arg ->
       if one_package then
         match !package with
         | Some _ -> raise (Arg.Bad "There can only be one package.")
         | None -> package := Some arg
       else
         raise (Arg.Bad "No package expected."))
    (usage ~one_package)

let print_usage ~one_package =
  Arg.usage (speclist ~one_package) (usage ~one_package)

let check_values ~one_package =
  if not Sys.(file_exists !contents) then
    raise (Arg.Bad (spf "Contents file (%s) must exist." !contents));
  if not (!cpu_timeout > 0.) then
    raise (Arg.Bad (spf "CPU timeout (%.0f) must be positive." !cpu_timeout));
  if not Sys.(file_exists !external_sources && is_directory !external_sources) then
    raise (Arg.Bad (spf "External sources directory (%s) must exist." !external_sources));
  if Sys.file_exists !report then
    raise (Arg.Bad (spf "Report directory (%s) must not exist." !report));
  if not (!workers > 0) then
    raise (Arg.Bad (spf "Workers number (%d) must be positive." !workers));
  if one_package then
    (
      if !package = None then
        raise (Arg.Bad (spf "Package expected."))
    )
  else
    (
      if not Sys.(file_exists !corpus && is_directory !corpus) then
        raise (Arg.Bad (spf "Corpus directory (%s) must exist." !corpus));
      if !package <> None then
        raise (Arg.Bad (spf "No package expected."))
    )

let symbolic_config =
  Colis.{
    prune_init_state = true ;
    loop_limit = 10 ;
    stack_size = 10
  }
