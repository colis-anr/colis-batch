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
let maintscripts pkg = pkg.maintscripts
let iter_maintscripts f pkg = List.iter f pkg.maintscripts

let maintscript pkg key = List.find_opt (Maintscript.has_key key) pkg.maintscripts

let parse ~content path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    failwith "Package.parse: no such directory";
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
  { path; name; version; content ; maintscripts }

let are_all_maintscripts_ok pkg =
  List.for_all
    (fun script -> not (Maintscript.has_error script))
    pkg.maintscripts
