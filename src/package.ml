open ExtPervasives

type name = string

type t =
  { name : name ;
    maintscripts : (Maintscript.name * Colis.colis option) list }

let name pkg = pkg.name
let maintscripts pkg = pkg.maintscripts

let parse_maintscripts ~package =
  let maintscripts =
    Maintscript.all_names
    |> List.map
      (fun maintscript ->
         let maintscript_str = Maintscript.name_to_string maintscript in
         let maintscript_path = Filename.(concat (concat !Options.corpus package) maintscript_str) in
         if Sys.file_exists maintscript_path then
           try
             let colis = Colis.parse_shell_file maintscript_path in
             Stats.(set_maintscript_parsing_status ~package ~maintscript Accepted);
             Some (maintscript, Some colis)
           with
             exn ->
             Stats.set_maintscript_parsing_status_from_exn ~package ~maintscript exn;
             None
         else
           (Stats.set_maintscript_absence ~package ~maintscript;
            Some (maintscript, None)))
  in
  if List.exists ((=) None) maintscripts then
    None
  else
    Some (List.map unwrap maintscripts)

let parse ~name =
  assert (Sys.file_exists (Filename.concat !Options.corpus name));
  match parse_maintscripts ~package:name with
  | None -> None
  | Some maintscripts ->
    Some { name ; maintscripts }
