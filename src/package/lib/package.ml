open Colis_ext

type name = string
type version = string

type t =
  { path : string ;
    name : name ;
    version : version ;
    maintscripts : Maintscript.t list }

let path pkg = pkg.path
let name pkg = pkg.name
let safe_name pkg = pkg.name ^ "_" ^ (soi (Hashtbl.hash pkg.path))
let version pkg = pkg.version
let maintscripts pkg = pkg.maintscripts
let iter_maintscripts f pkg = List.iter f pkg.maintscripts

let maintscript pkg key = List.find_opt (Maintscript.has_key key) pkg.maintscripts

let parse path =
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
  { path; name; version; maintscripts }

let are_all_maintscripts_ok pkg =
  List.for_all
    (fun script -> not (Maintscript.has_error script))
    pkg.maintscripts
