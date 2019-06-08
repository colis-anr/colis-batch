open Colis_ext

type 'a parsing =
  | ParsingErrored of string
  | ParsingRejected
  | ParsingAccepted of 'a

type 'a conversion =
  | ConversionErrored of string
  | ConversionRejected of string
  | ConversionAccepted of 'a

type maintscript = {
  mutable status : unit conversion parsing ;
}

let dummy_maintscript () = {
  status = ParsingErrored "not parsed yet" ;
}

type package = {
  mutable status : unit parsing ;
  mutable maintscripts : (Maintscript.Key.t * maintscript option) list ;
}

let dummy_package () = {
  status = ParsingErrored "not parsed yet" ;
  maintscripts = List.map (fun maintscript -> (maintscript, None)) Maintscript.all_names ;
}

let by_package : (string, package) Hashtbl.t =
  Hashtbl.create 10000

let get_package_stats ~name =
  match Hashtbl.find_opt by_package name with
  | None ->
    let package_stats = dummy_package () in
    Hashtbl.add by_package name package_stats;
    package_stats
  | Some package_stats -> package_stats

let get_maintscript_stats ~package ~name =
  let package_stats = get_package_stats ~name:package in
  match List.assoc name package_stats.maintscripts with
  | None ->
    let maintscript_stats = dummy_maintscript () in
    package_stats.maintscripts <- List.update_assoc name (Some maintscript_stats) package_stats.maintscripts;
    maintscript_stats
  | Some maintscript_stats ->
    maintscript_stats

let set_package_status ~package status =
  (get_package_stats ~name:package).status <- status

let set_maintscript_status ~package ~maintscript status =
  (get_maintscript_stats ~package ~name:maintscript).status <- status
