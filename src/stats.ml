type parsing_status =
  | Unexpected of string
  | MorbigRejected of string
  | ConversionRejected of string
  | Accepted

type script_stats = {
  mutable parsing_status : parsing_status
}

type package_stats = {
  mutable preinst  : script_stats option ;
  mutable postinst : script_stats option ;
  mutable prerm    : script_stats option ;
  mutable postrm   : script_stats option ;
}

let by_script : ((string * string), script_stats) Hashtbl.t = Hashtbl.create 30000
let by_package = Hashtbl.create 10000

let get_package_stats package =
  match Hashtbl.find_opt by_package package with
  | None ->
    let package_stats = {
      preinst = None ;
      postinst = None ;
      prerm = None ;
      postrm = None ;
    } in
    Hashtbl.add by_package package package_stats;
    package_stats
  | Some package_stats -> package_stats

let get_script_stats package script =
  let package_stats = get_package_stats package in
  let script_stats =
    match script with
    | "preinst"  -> package_stats.preinst
    | "postinst" -> package_stats.postinst
    | "prerm"    -> package_stats.prerm
    | "postrm"   -> package_stats.postrm
    | _ -> failwith "get_script_stats"
  in
  match script_stats with
  | None ->
    let script_stats = {
      parsing_status = Unexpected "has not been parsed" ;
    } in
    (match script with
     | "preinst"  -> package_stats.preinst  <- Some script_stats
     | "postinst" -> package_stats.postinst <- Some script_stats
     | "prerm"    -> package_stats.prerm    <- Some script_stats
     | "postrm"   -> package_stats.postrm   <- Some script_stats
     | _ -> assert false);
    Hashtbl.add by_script (package, script) script_stats;
    script_stats
  | Some script_stats ->
    script_stats

let set_parsing_status package script status =
  (get_script_stats package script).parsing_status <- status
