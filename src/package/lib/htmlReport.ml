open Colis_ext

let nb_states_to_report = 100 (* FIXME: option *)

let pp_viz fmt ?(id="noideafixme") file =
  fpf fmt {|
    <div id="viz-%s"></div>
    <script>
      var viz_client_%s = new XMLHttpRequest();
      var viz_%s = new Viz();

      viz_client_%s.open('GET', '%s');
      viz_client_%s.onreadystatechange = function() {
        if (viz_client_%s.readyState === 4){
          viz_%s.renderSVGElement(viz_client_%s.responseText)
            .then(function(element) {
              document.getElementById('viz-%s').appendChild(element);
            })
            .catch(error => {
              viz_%s = new Viz();
              console.error(error);
            });
        }
      }
      viz_client_%s.send();
    </script>
  |}
    id id id id file id id id id id id id

let pp_parsing_status fmt package =
  fpf fmt "<h2>Parsing</h2>";
  fpf fmt "<dl>";
  fpf fmt "<dt>Name</dt><dd>%s</dd>" (Package.name package);
  fpf fmt "<dt>Version</dt><dd>%s</dd>" (Package.version package);
  fpf fmt "<dt>Maintainer scripts</dt><dd><dl>";
  Package.iter_maintscripts
    (fun maintscript ->
       let (html_class, status, message) =
         (match Maintscript.error maintscript with
          | None -> ("accepted", "OK", "")
          | Some e ->
            match e with
            | ParsingErrored msg -> ("errored", "Error in parsing", msg)
            | ParsingRejected -> ("rejected", "Rejected by parsing", "")
            | ConversionErrored msg -> ("errored", "Error in conversion", msg)
            | ConversionRejected msg -> ("rejected", "Rejected by conversion", msg))
       in
       fpf fmt "<dt><a href=\"%s\">%s</a></dt><dd class=\"%s\">%s %s</dd>"
         ("script/" ^ Maintscript.key_as_string maintscript ^ ".html")
         (Maintscript.key_as_string maintscript) html_class status message)
    package;
  fpf fmt "</dl></dl>"

let pp_scenarii_summaries fmt scenarii =
  fpf fmt "<h2>Scenarii</h2>";
  if List.length scenarii > 0 then
    List.iter
      (fun (name, _) ->
         fpf fmt "<div><h3>%s</h3>" (Scenarii.Name.to_fancy_string name);
         let name = Scenarii.Name.to_string name in
         pp_viz fmt ~id:name
           (Filename.concat_l ["scenario"; name; "flowchart.dot"]);
         fpf fmt "<a href=\"%s\">Details</a></div>"
           (Filename.concat_l ["scenario"; name; "index.html"]))
      scenarii
  else
    fpf fmt "<div><p>No scenario could be run.</p></div>"

let pp_scenario fmt ran =
  pp_viz fmt "flowchart.dot";
  List.iter
    (fun (status, states) ->
       let status = Scenario.Status.to_string status in
       fpf fmt "<h2>%s</h2>" status;
       List.iteri
         (fun id _ ->
            if id < nb_states_to_report then
              (
                let id = string_of_int id in
                fpf fmt "<a href=\"%s\">%s</a> "
                  (Filename.concat_l [status; id ^ ".html"])
                  id
              )
            else if id = nb_states_to_report then
              fpf fmt "... and %d more." (List.length states - nb_states_to_report)
         )
         states)
    (Scenario.states ran);
  fpf fmt "</dl>"

let generate_and_write ?(prefix=[]) ~copy_static package scenarii =
  if copy_static then
    Colis_common.Report.copy_static_to ["static"];

  (
    Colis_common.Report.with_formatter_to_html_report
      ~title:(Package.name package)
      ~viz:true
      (prefix @ ["index.html"])
    @@ fun fmt ->
    pp_parsing_status fmt package;
    pp_scenarii_summaries fmt scenarii
  );
  Package.iter_maintscripts
    (fun maintscript ->
       Colis_common.Report.with_formatter_to_html_report
         ~title:(spf "%s – %s" (Package.name package) (Maintscript.key_as_string maintscript))
         ~highlight:true
         (prefix @ ["script"; Maintscript.key_as_string maintscript ^ ".html"])
       @@ fun fmt ->
       (
         let pp_status fmt msg =
           fpf fmt "<p><strong>Status:</strong> %s</p>" msg
         in
         match Maintscript.error maintscript with
         | None ->
           pp_status fmt "Accepted";
           fpf fmt "<h2>Colis script</h2><pre><code>";
           Colis.pp_print_colis fmt (Maintscript.colis maintscript);
           fpf fmt "</code></pre>"
         | Some error ->
           match error with
           | ParsingErrored msg ->
             pp_status fmt ("Parsing errored with: " ^ msg)
           | ParsingRejected ->
             pp_status fmt "Parsing rejected"
           | ConversionErrored msg ->
             pp_status fmt ("Conversion errored with: " ^ msg)
           | ConversionRejected msg ->
             pp_status fmt ("Conversion rejected with: " ^ msg)
       );
       (
         fpf fmt "<hr/><h2>Original Shell script</h2><pre><code class=\"bash\">";
         let ichan = open_in (Filename.concat_l [Package.path package; Maintscript.key_as_string maintscript]) in
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
       )

    )
    package;

  List.iter
    (fun (name, scenario) ->
       (
         Colis_common.Report.with_formatter_to_html_report
           ~title:(spf "%s – %s" (Package.name package) (Scenarii.Name.to_fancy_string name))
           ~viz:true
           (prefix @ ["scenario"; Scenarii.Name.to_string name; "index.html"])
         @@ fun fmt ->
         pp_scenario fmt scenario
       );
       (
         Colis_common.Report.with_formatter_to_file
           (prefix @ ["scenario"; Scenarii.Name.to_string name; "flowchart.dot"])
         @@ fun fmt ->
         Scenario.pp_ran_as_dot fmt scenario
       );
       (
         List.iter
           (fun (status, states) ->
              let status = Scenario.Status.to_string status in
              List.iteri
                (fun id state ->
                   if id < nb_states_to_report then
                     (
                       let id = string_of_int id in
                       (
                         Colis_common.Report.with_formatter_to_file
                           (prefix @ ["scenario"; Scenarii.Name.to_string name; status; id ^ ".dot"])
                         @@ fun fmt ->
                         let clause = state.Colis.Symbolic.Semantics.filesystem.clause in
                         Colis.Constraints.Clause.pp_sat_conj_as_dot
                           ~name:(Format.asprintf "%s-%s" status id)
                           fmt clause
                       );
                       (
                         Colis_common.Report.with_formatter_to_html_report
                           ~title:(spf "%s – %s – %s #%s" (Package.name package) (Scenarii.Name.to_fancy_string name) status id)
                           ~viz:true
                           (prefix @ ["scenario"; Scenarii.Name.to_string name; status; id ^ ".html"])
                         @@ fun fmt ->
                         pp_viz fmt (id ^ ".dot");
                         fpf fmt "<pre>";
                         Colis.print_symbolic_state fmt state;
                         fpf fmt "</pre>"
                       )
                     )
                )
                states)
           (Scenario.states scenario)
       )
    )
    scenarii
