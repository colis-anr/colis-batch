open Colis_batch_ext

type t =
  { mutable workers : int ;
    mutable cpu_timeout : float ;
    mutable memory_limit : string ;
    mutable report : string ;
    mutable external_sources : string ;
    mutable package : string ;
    mutable corpus : string ;
    mutable contents : string }
[@@deriving yojson {exn=true}]

let default () =
  { workers = 2 ;
    cpu_timeout = 5. ;
    memory_limit = "2G" ;
    report = "report" ;
    external_sources = "" ;
    package = "" ;
    corpus = "" ;
    contents = "" }

let config = ref (default ())
let default = default ()

let save_config_to_file file =
  !config |> to_yojson |> Yojson.Safe.to_file file

let load_config_from_file file =
  file |> Yojson.Safe.from_file |> of_yojson_exn |> (:=) config

let pp_arg_list = Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_string)

let speclist =
  Arg.[
    "--contents",
    String (fun s -> !config.contents <- s),
    spf "FILE Sets the path to the contents file (default: %s)" default.contents;

    "--cpu-timeout",
    Float (fun f -> !config.cpu_timeout <- f),
    spf "NB Sets the CPU timeout, in seconds (default: %.0f)" default.cpu_timeout;

    "--memory-limit",
    String (fun s -> !config.memory_limit <- s),
    spf "NB Sets the memory limit, in bytes (default: %s)" default.memory_limit;

    "--external-sources",
    String (fun s -> !config.external_sources <- s),
    spf "DIR Sets the path to the external sources (default: %s)" default.external_sources;

    "--package",
    String (fun s -> !config.package <- s),
    spf "DIR Sets the path to the package to analyse (default: %s)" default.external_sources;

    "--corpus",
    String (fun s -> !config.corpus <- s),
    spf "DIR Sets the path to the corpus (default: %s)" default.external_sources;

    "--report",
    String (fun s -> !config.report <- s),
    spf "DIR Sets the path to the report (default: %s)" default.report;

    "--workers",
    Int (fun i -> !config.workers <- i),
    spf "NB Sets the number of workers (default: %d)" default.workers;

    "--save-config",
    String save_config_to_file,
    spf "FILE Save the configuration to FILE";

    "--load-config",
    String load_config_from_file,
    spf "FILE Load the configuration from FILE";
  ]
  |> List.sort compare |> Arg.align

let check_config () =
  if not (!config.contents = "") && not (Sys.file_exists !config.contents) then
    raise (Arg.Bad (spf "Contents file (%s), when set, must exist." !config.contents));

  if not (!config.cpu_timeout > 0.) then
    raise (Arg.Bad (spf "CPU timeout (%.0f) must be positive." !config.cpu_timeout));

  if not (!config.external_sources = "") && not Sys.(file_exists !config.external_sources && is_directory !config.external_sources) then
    raise (Arg.Bad (spf "External sources (%s), when set, must exist and be a directory." !config.external_sources));

  if Sys.file_exists !config.report then
    raise (Arg.Bad (spf "Report directory (%s) must not exist." !config.report));

  if not (!config.workers > 0) then
    raise (Arg.Bad (spf "Workers number (%d) must be positive." !config.workers));

  if (!config.package = "") <> (!config.corpus = "") then
    raise (Arg.Bad (spf "Package and Corpus cannot be both set or unset at the same time."));

  if not (!config.package = "") && not Sys.(file_exists !config.package) then
    raise (Arg.Bad (spf "Package (%s), when set, must exist." !config.package));

  if not (!config.corpus = "") && not Sys.(file_exists !config.corpus && is_directory !config.corpus) then
    raise (Arg.Bad (spf "Corpus (%s), when set, must exist and be a directory." !config.corpus))

let () =
  try
    Arg.parse speclist
      (fun _ -> raise (Arg.Bad "No anonymous argument can be given"))
      Sys.argv.(0);
    check_config ()
  with
    Arg.Bad msg ->
    print_endline msg;
    print_newline ();
    Arg.usage speclist Sys.argv.(0);
    exit 1
