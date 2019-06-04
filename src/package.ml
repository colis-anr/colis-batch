open ExtPervasives

type name = string

type t =
  { name : name ;
    maintscripts : (Maintscript.name * Morsmall.AST.program option) list }

let name pkg = pkg.name
let maintscripts pkg = pkg.maintscripts
let maintscript pkg name = List.assoc name pkg.maintscripts

let parse_maintscripts ~package =
  let maintscripts =
    Maintscript.all_names
    |> List.map
      (fun maintscript ->
         let maintscript_str = Maintscript.name_to_string maintscript in
         let maintscript_path = Filename.(concat (concat !Options.corpus package) maintscript_str) in
         if Sys.file_exists maintscript_path then
           (
             let report_path = ["package"; package; "script"; maintscript_str ^ ".html"] in
             HtmlReport.with_formatter_to_report report_path @@ fun fmt ->
             let output =
               try
                 let shell = Morsmall.parse_file maintscript_path in
                 try
                   (* We try to convert with dummy arguments to see if we really can. *)
                   let colis = Colis.Language.FromShell.program__to__program ~cmd_line_arguments:["DUM"; "MY"] shell in
                   Stats.(set_maintscript_status ~package ~maintscript (ParsingAccepted (ConversionAccepted ())));
                   HtmlReport.Script.pp_accepted fmt colis;
                   Some (maintscript, Some shell)
                 with
                 | Colis.Errors.ConversionError msg ->
                   Stats.(set_maintscript_status ~package ~maintscript (ParsingAccepted (ConversionRejected msg)));
                   HtmlReport.Script.pp_conversion_rejected fmt msg;
                   None
                 | exn ->
                   let msg = Printexc.to_string exn in
                   Stats.(set_maintscript_status ~package ~maintscript (ParsingAccepted (ConversionErrored msg)));
                   HtmlReport.Script.pp_conversion_errored fmt msg;
                   None
               with
               | Morsmall.SyntaxError _pos ->
                 Stats.(set_maintscript_status ~package ~maintscript (ParsingRejected));
                 HtmlReport.Script.pp_parsing_rejected fmt ();
                 None
               | exn ->
                 let msg = Printexc.to_string exn in
                 Stats.(set_maintscript_status ~package ~maintscript (ParsingErrored msg));
                 HtmlReport.Script.pp_parsing_errored fmt msg;
                 None
             in
             HtmlReport.Script.pp_content fmt ~package maintscript_str;
             output
           )
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
