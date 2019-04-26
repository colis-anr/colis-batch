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
             let shell = Morsmall.parse_file maintscript_path in
             try
               let colis = Colis.Language.FromShell.program__to__program shell in
               Stats.(set_maintscript_status ~package ~maintscript (ParsingAccepted (ConversionAccepted ())));
               Some (maintscript, Some colis)
             with
             | exn ->
               Stats.(set_maintscript_status ~package ~maintscript (ParsingAccepted (
                   match exn with
                   | Colis.Errors.ConversionError msg -> ConversionRejected msg
                   | exn -> ConversionErrored (Printexc.to_string exn))));
               None
           with
           | exn ->
             Stats.(set_maintscript_status ~package ~maintscript (
                 match exn with
                 | Morsmall.SyntaxError _pos -> ParsingRejected
                 | exn -> ParsingErrored (Printexc.to_string exn)));
             None
         else
           (* No need to "set" the absence, as this is the default. *)
           Some (maintscript, None))
  in
  if List.exists ((=) None) maintscripts then
    None
  else
    Some (List.map unwrap maintscripts)

let parse name =
  assert (Sys.file_exists (Filename.concat !Options.corpus name));
  match parse_maintscripts ~package:name with
  | None ->
    Stats.(set_package_status ~package:name ParsingRejected);
    None
  | Some maintscripts ->
    Stats.(set_package_status ~package:name (ParsingAccepted ()));
    Some { name ; maintscripts }
