open Colis_ext

let fpf = Format.fprintf
(* let foi = float_of_int *)

(* let percentage a b =
  100. *. (foi a) /. (foi b)
 *)
let pp_header ~title ?(highlight=false) ?(viz=false) fmt () =
  fpf fmt {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>%s</title>

        <meta charset="utf-8" />
        <style type="text/css">
          body { width: 80%%; margin: auto; }
          .hidden { display: none; }
          .accepted { background: #afa; }
          .rejected { background: #aaf; }
          .errored  { background: #faa; }
          .empty    { background: #ddd; }
          pre { max-width: 100%%; overflow: auto; }
        </style>
    |}
    title;
  if highlight then
    fpf fmt {|
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/styles/github.min.css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/highlight.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"></script>
        <script>
          hljs.initHighlightingOnLoad();
          hljs.initLineNumbersOnLoad();
        </script>
      |};
  if viz then
    fpf fmt {|
        <script src="https://github.com/mdaines/viz.js/releases/download/v2.1.2/viz.js"></script>
        <script src="https://github.com/mdaines/viz.js/releases/download/v2.1.2/full.render.js"></script>
      |};
  fpf fmt {|
      </head>
      <body>
        <h1>%s</h1>
    |}
    title

let pp_footer fmt () =
  fpf fmt "</body></html>"

let rec ensure_existence path =
  let dir = Filename.dirname path in
  if dir <> path then ensure_existence dir;
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let with_formatter_to_file ?(relative=false) path f =
  let path = if relative then path else !Colis_config.report :: path in
  let path = Filename.concat_l path in
  ensure_existence (Filename.dirname path);
  let ochan = open_out path in
  let fmt = Format.formatter_of_out_channel ochan in
  let y = f fmt in
  Format.pp_print_flush fmt ();
  close_out ochan;
  y

let with_formatter_to_report ?(title="CoLiS-Covering Report") ?highlight ?viz ?relative path f =
  with_formatter_to_file ?relative path @@ fun fmt ->
  pp_header ~title ?highlight ?viz fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y

let pp_viz fmt ?(id="jaipasdidee") file =
  fpf fmt {|
    <div id="viz-%s"></div>
    <script>
      var client_%s = new XMLHttpRequest();
      var viz_%s = new Viz();

      client_%s.open('GET', '%s');
      client_%s.onreadystatechange = function() {
        if (client_%s.readyState === 4){
          viz_%s.renderSVGElement(client_%s.responseText)
            .then(function(element) {
              document.getElementById('viz-%s').appendChild(element);
            })
            .catch(error => {
              viz_%s = new Viz();
              console.error(error);
            });
        }
      }
      client_%s.send();
    </script>
  |}
    id id id id
    file
    id id id id id id id

module Package = struct
  let pp_parsing_status _ _ = () (* FIXME *)
  (* let pp_parsing_status fmt package =
    let package_stats = Stats.get_package_stats ~name:package in
    fpf fmt "<h2>Parsing</h2><table><tr><th>Maintscript</th><th>Status</th><th>Message</th></tr>";
    List.iter
      (fun (maintscript, maintscript_stats) ->
         match maintscript_stats with
         | Some maintscript_stats ->
           let (class_, status, message) =
             match (maintscript_stats : Stats.maintscript).Stats.status with
             | Stats.ParsingErrored msg -> ("errored", "Error in parsing", msg)
             | Stats.ParsingRejected -> ("rejected", "Parsing rejected", "")
             | Stats.ParsingAccepted status ->
               match status with
               | Stats.ConversionErrored msg -> ("errored", "Error in conversion", msg)
               | Stats.ConversionRejected msg -> ("rejected", "Conversion rejected", msg)
               | Stats.ConversionAccepted () -> ("accepted", "Accepted", "")
           in
           fpf fmt "<tr class=\"%s\"><td><a href=\"%s\">%s</a></td><td>%s</td><td>%s</td></tr>"
             class_
             (Filename.concat_l ["script"; (Maintscript.name_to_string maintscript) ^ ".html"])
             (Maintscript.name_to_string maintscript)
             status message
         | None ->
           fpf fmt "<tr class=\"empty\"><td>%s</td><td>Absent</td><td></td></tr>"
             (Maintscript.name_to_string maintscript))
      package_stats.maintscripts;
    fpf fmt "</table>" *)

  let pp_scenarii fmt _package =
    fpf fmt "<h2>Scenarii</h2>";
    List.iter
      (fun (name, _) ->
         let name = Scenario.name_to_string name in
         fpf fmt "<h3>%s</h3>" name;
         pp_viz fmt ~id:name
           (Filename.concat_l ["scenario"; name; "flowchart.dot"]);
         fpf fmt "<a href=\"%s\">Details</a>"
           (Filename.concat_l ["scenario"; name; "index.html"]))
      Scenarii.all
end

module Scenario = struct
  let pp_package fmt ~package scenario ran =
    ignore package;
    fpf fmt "<h1>%s</h1>" scenario;
    pp_viz fmt "flowchart.dot";
    let pp_status = Scenario.Status.pp in
    List.iter
      (fun (status, states) ->
         fpf fmt "<h2>%a</h2>" pp_status status;
         List.iteri
           (fun id _ ->
              fpf fmt "<a href=\"%s\">%d</a> "
                (Filename.concat_l [Scenario.Status.to_string status; (string_of_int id) ^ ".html"])
                id)
           states)
      ran;
    fpf fmt "</dl>"

  let pp_state fmt ~package ~status ~id state =
    ignore package;
    let pp_status = Scenario.Status.pp in
    fpf fmt "<h1>%a %d</h1>" pp_status status id;
    pp_viz fmt (string_of_int id ^ ".dot");
    fpf fmt "<pre>";
    Colis.print_symbolic_state fmt state;
    fpf fmt "</pre>"
end

module Script = struct
  let pp_content fmt ~package script =
    fpf fmt "<hr/><h2>Original Shell script</h2><pre><code class=\"bash\">";
    let ichan = open_in (Filename.concat_l [!Colis_config.corpus; package; script]) in
    let buflen = 1024 in
    let buf = Bytes.create buflen in
    let rec copy_all () =
      match input ichan buf 0 buflen with
      | 0 -> ()
      | n ->
        fpf fmt "%s" (Bytes.sub_string buf 0 n);
        copy_all ()
    in
    copy_all ();
    close_in ichan;
    fpf fmt "</code></pre>"

  let pp_status fmt msg =
    fpf fmt "<p><strong>Status:</strong> %s</p>" msg

  let pp_accepted fmt colis =
    pp_status fmt "Accepted";
    fpf fmt "<h2>Colis script</h2><pre><code>";
    fpf fmt "@[<h>%a@]@?" Colis.pp_print_colis colis;
    fpf fmt "</code></pre>"

  let pp_conversion_rejected fmt msg =
    pp_status fmt ("Conversion rejected with: " ^ msg)

  let pp_conversion_errored fmt msg =
    pp_status fmt ("Conversion errored with: " ^ msg)

  let pp_parsing_rejected fmt () =
    pp_status fmt "Parsing rejected"

  let pp_parsing_errored fmt msg =
    pp_status fmt ("Parsing errored with: " ^ msg)
end

(* let pp fmt () =
  let () =
    let nb_all = ref 0 in
    let nb_accepted = ref 0 in
    let nb_rejected = ref 0 in
    Hashtbl.iter
      (fun _ package_stats ->
         incr nb_all;
         match package_stats.Stats.status with
         | ParsingErrored _ -> assert false
         | ParsingRejected -> incr nb_rejected
         | ParsingAccepted _ -> incr nb_accepted)
      Stats.by_package;
    fpf fmt {|
        <h2>Summary for packages</h2>
        <ul>
          <li>All: %d</li>
          <li>Accepted: %d (%.2f%% of all)</li>
          <li>Rejected/errored: %d (%.2f%% of all)</li>
        </ul>
      |}
      !nb_all
      !nb_accepted (percentage !nb_accepted !nb_all)
      !nb_rejected (percentage !nb_rejected !nb_all)
  in
  let () =
    let nb_all = ref 0 in
    let nb_parsing_accepted = ref 0 in
    let nb_parsing_rejected = ref 0 in
    let nb_parsing_errored = ref 0 in
    let nb_conversion_accepted = ref 0 in
    let nb_conversion_rejected = ref 0 in
    let nb_conversion_errored = ref 0 in
    Hashtbl.iter
      (fun _ package_stats ->
         List.iter
           (fun (_, maintscript_stats) ->
              match maintscript_stats with
              | None -> ()
              | Some maintscript_stats ->
                incr nb_all;
                match (maintscript_stats : Stats.maintscript).status with
                | ParsingErrored _ -> incr nb_parsing_errored
                | ParsingRejected -> incr nb_parsing_rejected
                | ParsingAccepted status ->
                  incr nb_parsing_accepted;
                  match status with
                  | ConversionErrored _ -> incr nb_conversion_errored
                  | ConversionRejected _ -> incr nb_conversion_rejected
                  | ConversionAccepted () -> incr nb_conversion_accepted)
           package_stats.Stats.maintscripts)
      Stats.by_package;
    let hidden = function
      | 0 -> "hidden"
      | _ -> ""
    in
    fpf fmt {|
        <h2>Summary for maintainer scripts</h2>
        <ul>
          <li>All: %d</li>
        </ul>
        <table>
          <tr>
            <th>Parsing</th>
            <th>Conversion</th>
          </tr>
          <tr>
            <td rowspan="3" class="accepted %s">Accepted<br/>%d (%0.2f%% of all)</td>
            <td class="accepted %s">Accepted<br/>%d (%0.2f%% of all; %0.2f%% of parsed)</td>
          </tr>
          <tr>
            <td class="rejected %s">Rejected<br/>%d (%0.2f%% of all; %0.2f%% of parsed)</td>
          </tr>
          <tr>
            <td class="errored %s">Errored<br/>%d (%0.2f%% of all; %0.2f%% of parsed)</td>
          </tr>
          <tr>
            <td class="rejected %s">Rejected<br/>%d (%0.2f%% of all)</td>
            <td class="empty"></td>
          </tr>
          <tr>
            <td class="errored %s">Errored<br/>%d (%0.2f%% of all)</td>
            <td class="empty"></td>
          </tr>
        </table>
      |}
      !nb_all
      (hidden !nb_parsing_accepted)    !nb_parsing_accepted    (percentage !nb_parsing_accepted !nb_all)
      (hidden !nb_conversion_accepted) !nb_conversion_accepted (percentage !nb_conversion_accepted !nb_all) (percentage !nb_conversion_accepted !nb_parsing_accepted)
      (hidden !nb_conversion_rejected) !nb_conversion_rejected (percentage !nb_conversion_rejected !nb_all) (percentage !nb_conversion_rejected !nb_parsing_accepted)
      (hidden !nb_conversion_errored)  !nb_conversion_errored  (percentage !nb_conversion_errored !nb_all)  (percentage !nb_conversion_errored !nb_parsing_accepted)
      (hidden !nb_parsing_rejected)    !nb_parsing_rejected    (percentage !nb_parsing_rejected !nb_all)
      (hidden !nb_parsing_errored)     !nb_parsing_errored     (percentage !nb_parsing_errored !nb_all)
  in
  () *)

(* ************************************************************************** *)
(* Now that we have written prettys-printer, we generate an arborescence of
   HTML file with them. *)

(* let with_magic_formatter ~path f =
  ignore (Sys.command ("mkdir -p " ^ (Filename.dirname path)));
  let out = open_out path in
  let fmt = Format.formatter_of_out_channel out in
  let a = try Ok (f fmt) with exn -> Error exn in
  fpf fmt "@?"; close_out out;
  match a with Ok x -> x | Error e -> raise e *)

(* let gaw_package package =
  let path = ExtFilename.concat_l [!Options.report; "package"; package; "index.html"] in
  with_magic_formatter ~path @@ fun fmt ->
  fpf fmt "%s%a%s" (header ~title:"CoLiS-Language Covering") pp_package package footer
  (* FIXME: generate pages for maintscripts *)

let gaw_packages () =
  let path = ExtFilename.concat_l [!Options.report; "package"; "index.html"] in
  with_magic_formatter ~path @@ fun fmt ->
  fpf fmt "%s<ul>" (header ~title:"FIXME");
  Hashtbl.iter
    (fun package _ ->
       gaw_package package;
       fpf fmt "<li><a href=\"%s\">%s</a></li>" package package)
    Stats.by_package;
  fpf fmt "</ul>%s" footer

let gaw () =
  let path = Filename.concat !Options.report "index.html" in
  with_magic_formatter ~path @@ fun fmt ->
  fpf fmt "%s%a%s" (header ~title:"CoLiS-Language Covering") pp () footer;
  gaw_packages () *)

let generate_and_write () = ()
