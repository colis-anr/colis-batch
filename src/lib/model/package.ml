open Colis_batch_ext

type name = string  [@@deriving yojson]
type version = string  [@@deriving yojson]

type t =
  { path : string ;
    name : name ;
    version : version ;
    content : string list ;
    maintscripts : Maintscript.t list }
[@@deriving yojson]

let path pkg = pkg.path
let name pkg = pkg.name
let safe_name pkg = pkg.name ^ "_" ^ (soi (Hashtbl.hash pkg.path))
let version pkg = pkg.version
let content pkg = pkg.content
let maintscripts pkg = pkg.maintscripts
let iter_maintscripts f pkg = List.iter f pkg.maintscripts

let maintscript pkg key = List.find_opt (Maintscript.has_key key) pkg.maintscripts

let parse path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    failwith "Package.parse_from_dir: no such directory";
  if not (Sys.file_exists (Filename.concat path "content")) then
    failwith "Package.parse_from_dir: no content file";

  let (name, version) = String.split_2_on_char '_' (Filename.basename path) in
  let maintscripts =
    Maintscript.Key.all
    |> List.map_filter
      (fun maintscript_name ->
         let maintscript_path = Filename.concat path (Maintscript.Key.to_string maintscript_name) in
         if Sys.file_exists maintscript_path then
           Some (Maintscript.parse maintscript_path)
         else
           None)
  in
  let content =
    Filename.concat path "content"
    |> Filesystem.read_lines_from_file
    |> List.map (fun s -> "/" ^ s)
  in
  { path; name; version; content ; maintscripts }

let are_all_maintscripts_ok pkg =
  List.for_all
    (fun script -> not (Maintscript.has_error script))
    pkg.maintscripts
