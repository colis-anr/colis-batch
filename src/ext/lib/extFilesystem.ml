exception FileExists of string
exception NoSuchFile of string

let mkdir_one mode path =
  try
    Unix.mkdir path mode
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> raise (FileExists path)
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise (NoSuchFile path)

let rec mkdir_parents mode = function
  | "." | "/" -> ()
  | path ->
    mkdir_parents mode (Filename.dirname path);
    try
      mkdir_one mode path
    with
      FileExists path -> (* weirder than if, but atomic *)
      if not (Sys.is_directory path) then
        raise (FileExists path)
      else
        ()

let mkdir ?(mode=0o777) ?(parents=false) path =
  (if parents then mkdir_parents else mkdir_one) mode path

let write_to_file ~content path =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_lines_from_file path =
  let ic = open_in path in
  let rec read_lines_from_file lines =
    try
      read_lines_from_file (input_line ic :: lines)
    with
      End_of_file -> List.rev lines
  in
  let lines = read_from_file () in
  close_in ic;
  lines
