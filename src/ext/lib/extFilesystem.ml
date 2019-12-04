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
