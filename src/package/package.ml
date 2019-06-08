open Colis_ext

type name = string
type version = string

type t =
  { name : name ;
    version : version ;
    maintscripts : (Maintscript.Key.t * Maintscript.t) list }

let name pkg = pkg.name
let version pkg = pkg.version
let maintscript pkg name = List.assoc name pkg.maintscripts

let parse path =
  let (name, version) = String.split_2_on_char '_' (Filename.basename path) in
  let maintscripts =
    Maintscript.Key.all
    |> List.map
      (fun maintscript_name ->
         let maintscript_path = Filename.concat path (Maintscript.Key.to_string maintscript_name) in
         (maintscript_name, Maintscript.parse maintscript_path))
  in
  { name; version; maintscripts }
