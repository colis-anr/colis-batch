let spf = Format.sprintf

let corpus = ref "corpus"
let cpu_timeout = ref 5.
let external_sources = ref "external_sources"
let report = ref "report"
let workers = ref 2

let speclist =
  Arg.(align [
      "--corpus",      Set_string corpus,      spf "DIR Sets the path to the corpus (default: %s)" !corpus;
      "--cpu-timeout", Set_float  cpu_timeout, spf "NB Sets the CPU timeout, in seconds (default: %.0f)" !cpu_timeout;
      "--external-sources", Set_string external_sources, spf "DIR Sets the path to the external sources (default: %s)" !external_sources;
      "--report",      Set_string report,      spf "DIR Sets the path to the report (default: %s)" !report;
      "--workers",     Set_int    workers,     spf "NB Sets the number of workers (default: %d)" !workers;
    ])
let usage = spf "%s [OPTIONS]" Sys.argv.(0)

let parse_command_line () =
  Arg.parse
    speclist
    (fun arg -> raise (Arg.Bad (spf "Unexpected argument: %s" arg)))
    usage

let print_usage () =
  Arg.usage speclist usage

let check_values () =
  if not Sys.(file_exists !corpus && is_directory !corpus) then
    raise (Arg.Bad (spf "Corpus directory (%s) must exist." !corpus));
  if not (!cpu_timeout > 0.) then
    raise (Arg.Bad (spf "CPU timeout (%.0f) must be positive." !cpu_timeout));
  if Sys.(file_exists !external_sources && is_directory !external_sources) then
    raise (Arg.Bad (spf "External sources directory (%s) must exist." !external_sources));
  if Sys.file_exists !report then
    raise (Arg.Bad (spf "Report directory (%s) must not exist." !report));
  if not (!workers > 0) then
    raise (Arg.Bad (spf "Workers number (%d) must be positive." !workers))

let symbolic_config =
  Colis.{
    prune_init_state = true ;
    loop_limit = 10 ;
    stack_size = 10
  }
