open Colis_batch_ext
module Model = Colis_batch_model
open Colis_batch_report_common.Package

let nb_states_to_report = 100 (* FIXME: should not be hardcoded *)

let pp_maintscript_status fmt maintscript =
  let (html_class, status, message) =
    (match Model.Maintscript.error maintscript with
     | None -> ("accepted", "OK", "")
     | Some e ->
       match e with
       | ParsingErrored msg -> ("errored", "Error in parsing", msg)
       | ParsingRejected _pos -> ("rejected", "Rejected by parsing", "") (* FIXME: pos *)
       | ConversionErrored msg -> ("errored", "Error in conversion", msg)
       | ConversionRejected (_pos, msg) -> ("rejected", "Rejected by conversion", msg)) (* FIXME: pos *)
  in
  fpf fmt "<dt><a href=\"%s\">%s</a></dt><dd class=\"%s\">%s %s</dd>"
    ("script/" ^ Model.Maintscript.key_as_string maintscript ^ ".html")
    (Model.Maintscript.key_as_string maintscript) html_class status message

let pp_parsing_status fmt (report : t) =
  fpf fmt "<dl>";
  fpf fmt "<dt>Name</dt><dd>%s</dd>" (Model.Package.name report.package);
  fpf fmt "<dt>Version</dt><dd>%s</dd>" (Model.Package.version report.package);
  fpf fmt "<dt>Maintainer scripts</dt>";
  fpf fmt "<dd><dl>";
  Model.Package.iter_maintscripts (pp_maintscript_status fmt) report.package;
  fpf fmt "</dl></dl>"

let pp_scenario_summary fmt (name, scenario) =
  fpf fmt "<div style=\"clear: left;\"><h3>%s</h3>" (Model.Scenarii.Name.to_fancy_string name);
  let name = Model.Scenarii.Name.to_string name in

  fpf fmt "<div style=\"float: left;\">%a</div>"
    Common.pp_viz
    (Filename.concat_l ["scenario"; name; "flowchart.dot"]);

  List.iter
    (fun (status, states) ->
       let status = Model.Scenario.Status.to_string status in
       fpf fmt "<h4>%s</h4>" status;
       List.iteri
         (fun id _ ->
            if id < nb_states_to_report then
              (
                let id = string_of_int id in
                fpf fmt "<a href=\"%s\">%s</a> "
                  (Filename.concat_l ["scenario"; name; status; id ^ ".html"])
                  id
              )
            else if id = nb_states_to_report then
              fpf fmt "... and %d more." (List.length states - nb_states_to_report)
         )
         states)
    (Model.Scenario.states scenario);

  (
    let open Model.Scenario in
    match coverage scenario with
    | Complete -> ()
    | Partial r | Null r ->
      List.iter
        (fun (utility, message) ->
           fpf fmt "<p>Unknown: %s: %s</p>" utility message)
        (ran_node_gen_unknown r);
      List.iter
        (fpf fmt "<p>Unexpected exception: %s</p>")
        (ran_node_gen_unexpected r)
  );

  fpf fmt "</div>"

let pp_scenario_summaries fmt (report : t) =
  List.iter (pp_scenario_summary fmt) report.scenarii

let pp_index ~standalone fmt (report : t) =
  if standalone then
    (fpf fmt "<h2>Configuration</h2>";
     Config.pp fmt report.config);
  fpf fmt "<h2>Meta</h2>";
  Meta.pp fmt report.meta;
  fpf fmt "<h2>Parsing Status</h2>";
  pp_parsing_status fmt report;
  fpf fmt "<h2>Scenarios Summaries</h2>";
  pp_scenario_summaries fmt report

let pp_redirecting_index fmt tap =
  let path = Common.path_from_tap tap |> Filename.concat_l in
  fpf fmt "<meta http-equiv=\"refresh\" content=\"0; url=%s\" />"
    path;
  fpf fmt "<p>This is a report for a single package. There is nothing here to
    see. You should be redirected. If not, you may <a href=\"%s\">use this
    link</a>.</p>"
    path

let pp_maintscript_colis fmt maintscript =
  let pp_status fmt msg =
    fpf fmt "<p><strong>Status:</strong> %s</p>" msg
  in
  match Model.Maintscript.error maintscript with
  | None ->
    pp_status fmt "Accepted";
    fpf fmt "<h2>Colis script</h2><pre><code>";
    Colis.Language.pp_print_colis fmt (Model.Maintscript.colis maintscript);
    fpf fmt "</code></pre>";
    fpf fmt "<p>Present utilities: %a.</p>"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> fpf fmt ", ")
         (fun fmt util -> fpf fmt "<code>%s</code>" util))
      (Model.Maintscript.utilities maintscript |> List.map fst)

  | Some error ->
    match error with
    | ParsingErrored msg ->
      pp_status fmt ("Parsing errored with: " ^ msg)
    | ParsingRejected _pos ->
      pp_status fmt "Parsing rejected" (* FIXME: pos *)
    | ConversionErrored msg ->
      pp_status fmt ("Conversion errored with: " ^ msg)
    | ConversionRejected (_pos, msg) ->
      pp_status fmt ("Conversion rejected with: " ^ msg) (* FIXME: pos *)

let pp_maintscript_shell fmt (report : t) maintscript =
  fpf fmt "<hr/><h2>Original Shell script</h2><pre><code class=\"bash\">";
  let ichan = open_in (Filename.concat_l [Model.Package.path report.package; Model.Maintscript.key_as_string maintscript]) in
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

let generate_maintscript ~prefix tap_package (report : t) maintscript =
  Common.with_formatter_to_html_report
    ~highlight:true ~prefix
    (tap_package @ [
        Model.Maintscript.key_as_string maintscript,
        ["script"; Model.Maintscript.key_as_string maintscript ^ ".html"]])
  @@ fun fmt ->
  pp_maintscript_colis fmt maintscript;
  pp_maintscript_shell fmt report maintscript

let generate_scenario_dot ~prefix tap_package name scenario =
  Colis_batch_report_common.with_formatter_to_file ~prefix
    (Common.path_from_tap (tap_package @ ["DUMMY", ["scenario"; Model.Scenarii.Name.to_string name; "flowchart.dot"]]))
  @@ fun fmt ->
  Model.Scenario.pp_ran_as_dot fmt scenario

let generate_scenario_state ~prefix tap_package name id state status =
  let status = Model.Scenario.Status.to_string status in
  let id = string_of_int id in
  (
    Colis_batch_report_common.with_formatter_to_file ~prefix
      (Common.path_from_tap (tap_package @ ["DUMMY", ["scenario"; Model.Scenarii.Name.to_string name; status; id ^ ".dot"]]))
    @@ fun fmt ->
    let clause = state.Colis.SymbolicConstraints.filesystem.clause in
    Colis_constraints.Clause.pp_sat_conj_as_dot
      ~name:(Format.asprintf "%s-%s" status id)
      fmt clause
  );
  (
    Common.with_formatter_to_html_report ~viz:true ~prefix
      (tap_package @ [
          Model.Scenarii.Name.to_fancy_string name, ["scenario"; Model.Scenarii.Name.to_string name; "index.html"];
          status ^ " #" ^ id, [status; id ^ ".html"]])
    @@ fun fmt ->
    Common.pp_viz fmt (id ^ ".dot");
    let open Colis in
    let open SymbolicConstraints in
    if not (Common.Stdout.is_empty state.stdout) then
      (
        fpf fmt "<h2>stdout</h2><pre>";
        List.iter (fpf fmt "%s@\n")
          (List.rev @@ state.stdout.lines);
        if state.stdout.line <> "" then
          fpf fmt "%s" state.stdout.line;
        fpf fmt "</pre>"
      );
    if not (Common.Stdout.is_empty state.log) then
      (
        fpf fmt "<h2>log</h2><pre>";
        List.iter (fpf fmt "%s@\n")
          (List.rev @@ state.log.lines);
        if state.log.line <> "" then
          fpf fmt "%s" state.log.line;
        fpf fmt "</pre>"
      )
  )

let generate ~standalone ~prefix (report : t) =
  let tap = [Model.Package.name report.package,
             ["package"; Model.Package.safe_name report.package; "index.html"]] in
  if standalone then
    ( (* In case of standalone, we need to take care of the static extraction
         ourselves. And generate an index that redirects directly in the package. *)
      Common.extract_static ~prefix;
      Common.with_formatter_to_html_report ~prefix [] @@ fun fmt ->
      pp_redirecting_index fmt tap
    );
  ( (* Index of the package. *)
    Common.with_formatter_to_html_report ~viz:true ~prefix tap @@ fun fmt ->
    pp_index ~standalone fmt report
  );
  (* One page for each maintscript. *)
  Model.Package.iter_maintscripts
    (generate_maintscript ~prefix tap report)
    report.package;
  (* Scenario summary and states *)
  List.iter
    (fun (name, scenario) ->
       generate_scenario_dot ~prefix tap name scenario;
       List.iter
         (fun (status, states) ->
            List.iteri
              (fun id state ->
                 if id < nb_states_to_report then
                   generate_scenario_state ~prefix tap name id state status)
              states)
         (Model.Scenario.states scenario))
    report.scenarii
