type maintscript_parsing_status =
  | Unexpected of string
  | ParsingRejected of string
  | ConversionRejected of string
  | Accepted

type maintscript_stats = {
  mutable parsing_status : maintscript_parsing_status
}

let dummy_maintscript_stats = {
  parsing_status = Unexpected "not parsed yet" ;
}

type package_stats = {
  mutable maintscripts : (Maintscript.name * maintscript_stats option) list
}

let dummy_package_stats = {
  maintscripts = List.map (fun maintscript -> (maintscript, None)) Maintscript.all_names ;
}

let by_maintscript : (string * Maintscript.name, maintscript_stats) Hashtbl.t =
  Hashtbl.create 30000
let by_package : (string, package_stats) Hashtbl.t =
  Hashtbl.create 10000

let get_package_stats ~name =
  match Hashtbl.find_opt by_package name with
  | None ->
    Hashtbl.add by_package name dummy_package_stats;
    dummy_package_stats
  | Some package_stats -> package_stats

let get_maintscript_stats ~package ~name =
  let package_stats = get_package_stats ~name:package in
  match List.assoc name package_stats.maintscripts with
  | None ->
    package_stats.maintscripts <- ExtList.update_assoc name (Some dummy_maintscript_stats) package_stats.maintscripts;
    Hashtbl.add by_maintscript (package, name) dummy_maintscript_stats;
    dummy_maintscript_stats
  | Some maintscript_stats ->
    maintscript_stats

let set_maintscript_parsing_status ~package ~maintscript status =
  (get_maintscript_stats ~package ~name:maintscript).parsing_status <- status

let set_maintscript_parsing_status_from_exn ~package ~maintscript exn =
  set_maintscript_parsing_status ~package ~maintscript
    (match exn with
     | Colis.Errors.ParseError (msg, _pos) -> ParsingRejected msg
     | Colis.Errors.ConversionError msg -> ConversionRejected msg
     | exn -> Unexpected (Printexc.to_string exn))

let set_maintscript_absence ~package ~maintscript =
  let package_stats = get_package_stats ~name:package in
  package_stats.maintscripts <- ExtList.update_assoc maintscript None package_stats.maintscripts
