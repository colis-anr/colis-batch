let package_path name =
  ExtFilename.concat_l [!Options.report; "package"; name]

let scenario_path ~package ~scenario file =
  ExtFilename.concat_l [package_path package; "scenario"; scenario; file]

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
