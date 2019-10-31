let spf = Format.asprintf
let pp_arg_list = Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_string)

let def_contents = ["contents"]
let def_corpus = "corpus"
let def_cpu_timeout = 60.
let def_memory_limit = "2G"
let def_external_sources = "external_sources"
let def_report = "report"
let def_workers = 2

let contents = ref [] (* On purpose! *)
let corpus = ref def_corpus
let cpu_timeout = ref def_cpu_timeout
let memory_limit = ref def_memory_limit
let external_sources = ref def_external_sources
let package = ref None
let report = ref def_report
let workers = ref def_workers

let add l x = l := x :: !l

let speclist ~one_package =
  let speclist =
    Arg.[
      "--contents",    String (add contents),  spf "FILES Sets the path to the contents file (default: %a)" pp_arg_list def_contents;
      "--cpu-timeout", Set_float  cpu_timeout, spf "NB Sets the CPU timeout, in seconds (default: %.0f)" def_cpu_timeout;
      "--memory-limit", Set_string memory_limit, spf "NB Sets the memory limit, in bytes (default: %s)" def_memory_limit;
      "--external-sources", Set_string external_sources, spf "DIR Sets the path to the external sources (default: %s)" def_external_sources;
      "--report",      Set_string report,      spf "DIR Sets the path to the report (default: %s)" def_report;
      "--workers",     Set_int    workers,     spf "NB Sets the number of workers (default: %d)" def_workers;
    ]
  in
  let speclist =
    if not one_package then
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
    (usage ~one_package);
  contents := List.rev !contents;
  if !contents = [] then contents := def_contents

let print_usage ~one_package =
  Arg.usage (speclist ~one_package) (usage ~one_package)

let check_values ~one_package =
  if List.exists (fun content -> not Sys.(file_exists content)) !contents then
    raise (Arg.Bad (spf "All contents file (%a) must exist." pp_arg_list !contents));
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
