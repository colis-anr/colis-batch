let path file =
  Filename.concat !Options.report file

let or_id_if bool f =
  if bool then
    fun x -> x
  else
    f

let package_path ?(relative=false) ~package file =
  ExtFilename.concat_l ["package"; package; file]
  |> (path |> or_id_if relative)

let scenario_path ?(relative=false) ~package ~scenario file =
  ExtFilename.concat_l ["scenario"; scenario; file]
  |> (package_path ~relative ~package |> or_id_if relative)

let scripts_path ?(relative=false) ~package file =
  Filename.concat "script" file
  |> (package_path ~relative ~package |> or_id_if relative)

let rec ensure_existence path =
  let dir = Filename.dirname path in
  if dir <> path then ensure_existence dir;
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let with_formatter_to_file path f =
  ensure_existence (Filename.dirname path);
  let ochan = open_out path in
  let fmt = Format.formatter_of_out_channel ochan in
  let y = f fmt in
  Format.pp_print_flush fmt ();
  close_out ochan;
  y
