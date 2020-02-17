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

let with_output_channel_on_file path fun_ =
  let oc = open_out path in
  let v = try Ok (fun_ oc) with exn -> Error exn in
  close_out oc;
  match v with Ok v -> v | Error exn -> raise exn

let write_string_to_file ~content path =
  with_output_channel_on_file path @@ fun oc ->
  output_string oc content

let write_value_to_file value path =
  with_output_channel_on_file path @@ fun oc ->
  output_value oc value

let with_input_channel_on_file path fun_ =
  let ic = open_in path in
  let v = try Ok (fun_ ic) with exn -> Error exn in
  close_in ic;
  match v with Ok v -> v | Error exn -> raise exn

let read_lines_from_file path =
  with_input_channel_on_file path @@ fun ic ->
  let rec read_lines_from_file lines =
    try
      read_lines_from_file (input_line ic :: lines)
    with
      End_of_file -> List.rev lines
  in
  read_lines_from_file []

let read_value_from_file path =
  with_input_channel_on_file path @@ fun ic ->
  input_value ic
