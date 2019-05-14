let fpf = Format.fprintf
(* let foi = float_of_int *)

(* let percentage a b =
  100. *. (foi a) /. (foi b)
 *)
let pp_header ~title fmt () =
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
        </style>

        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/styles/github.min.css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/highlight.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"></script>
        <script>
          hljs.initHighlightingOnLoad();
          hljs.initLineNumbersOnLoad();
        </script>
      </head>
      <body>
        <h1>%s</h1>
    |}
    title title

let pp_footer fmt () =
  fpf fmt "</body></html>"

let with_formatter_to_report ?(title="CoLiS-Covering Report") path f =
  ReportHelpers.with_formatter_to_file path @@ fun fmt ->
  pp_header ~title fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y

module Package = struct
  let pp_parsing_status fmt package =
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
             (ReportHelpers.scripts_path ~relative:true ~package ((Maintscript.name_to_string maintscript)^".html"))
             (Maintscript.name_to_string maintscript)
             status message
         | None ->
           fpf fmt "<tr class=\"empty\"><td>%s</td><td>Absent</td><td></td></tr>"
             (Maintscript.name_to_string maintscript))
      package_stats.maintscripts;
    fpf fmt "</table>"

  let pp_scenarii fmt package =
    fpf fmt "<h2>Scenarii</h2>";
    List.iter
      (fun (name, _) ->
         let name = Scenario.name_to_string name in
         fpf fmt "<h3>%s</h3><img src=\"%s\"/>"
           name
           (ReportHelpers.scenario_path ~relative:true ~package ~scenario:name "flowchart.dot.png"))
      Scenarii.all
end

module Script = struct
  let pp_content fmt ~package script =
    fpf fmt "<pre><code class=\"bash\">";
    let ichan = open_in (ExtFilename.concat_l [!Options.corpus; package; script]) in
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

  let pp_accepted fmt () =
    pp_status fmt "Accepted"

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
