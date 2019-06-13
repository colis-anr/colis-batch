open Colis_ext

let generate_and_write_for_scenario ~prefix name packages =
  let scenario = List.assoc name Colis_package.Scenarii.all in
  (
    Colis_common.Report.with_formatter_to_file
      [prefix; "scenario"; Colis_package.Scenario.name_to_string name; "flowchart.dot"]
    @@ fun fmt ->
    Colis_package.Scenario.pp_unit_as_dot ~name fmt scenario
  );
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scenario Report"
    ~viz:true
    [prefix; "scenario"; Colis_package.Scenario.name_to_string name; "index.html"]
  @@ fun fmt ->
  Colis_common.Report.pp_viz fmt "flowchart.dot";
  let all_status = Colis_package.Scenario.all_status scenario in
  fpf fmt "<h2>Summary</h2><ul>";
  List.iter
    (fun status ->
       fpf fmt "<li><a href=\"#%a\">%a</a></li>"
         Colis_package.Scenario.Status.pp status
         Colis_package.Scenario.Status.pp status)
    all_status;
  fpf fmt "</ul>";
  List.iter
    (fun status ->
       fpf fmt "<h2 id=\"%a\">%a</h2>"
         Colis_package.Scenario.Status.pp status
         Colis_package.Scenario.Status.pp status;
       (* For each status, we list all the packages that, for the same scenario
          meet that status. *)
       List.iter
         (fun (package, scenarii) ->
            List.iter
              (fun (name', scenario) ->
                 if name' = name then
                   (
                     let states = Colis_package.Scenario.states scenario in
                     List.iter
                       (fun (status', states) ->
                          if status' = status && states <> [] then
                            fpf fmt "<li><a href=\"../../package/%s/index.html\">%s</a></li>"
                              (Colis_package.Package.name package)
                              (Colis_package.Package.name package)
                       )
                       states
                   )
              )
              scenarii
         )
         packages
    )
    all_status

let generate_and_write ~prefix ~time packages =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Report"
    ~viz:true
    [prefix; "index.html"]
  @@ fun fmt ->

  fpf fmt {|
    <h2>Meta</h2>
    <dl>
      <dt>Total time</dt><dd>%.0fs</dd>
    </dl>
  |}
    (floor (0.5 +. time));

  let packages_total = ref 0 in
  let packages_rejected = ref 0 in
  let packages_accepted = ref 0 in
  let scripts_total = ref 0 in
  let scripts_parsing_errored = ref 0 in
  let scripts_parsing_rejected = ref 0 in
  let scripts_conversion_errored = ref 0 in
  let scripts_conversion_rejected = ref 0 in
  let scripts_accepted = ref 0 in
  List.iter
    (fun (package, _) ->
       incr packages_total;
       let accepted = ref true in
       Colis_package.Package.iter_maintscripts
         (fun (_, maintscript) ->
            if Colis_package.Maintscript.is_present maintscript then
              (
                incr scripts_total;
                match Colis_package.Maintscript.has_error maintscript with
                | None -> incr scripts_accepted
                | Some error ->
                  accepted := false;
                  match error with
                  | ParsingErrored _ -> incr scripts_parsing_errored
                  | ParsingRejected -> incr scripts_parsing_rejected
                  | ConversionErrored _ -> incr scripts_conversion_errored
                  | ConversionRejected _ -> incr scripts_conversion_rejected
              )
         )
         package;
       if !accepted then
         incr packages_accepted
       else
         incr packages_rejected)
    packages;

  fpf fmt {|
    <h2>Parsing</h2>
    <dl>
      <dt>Packages</dt>
      <dd>
        <dl>
          <dt>Total</dt><dd>%d</dd>
          <dt>Rejected</dt><dd>%d</dd>
          <dt>Accepted</dt><dd>%d</dd>
        </dl>
      </dd>

      <dt>Scripts</dt>
      <dd>
        <dl>
          <dt>Total</dt><dd>%d</dd>
          <dt>Errored in parsing</dt><dd>%d</dd>
          <dt>Rejected by parsing</dt><dd>%d</dd>
          <dt>Errored in conversion</dt><dd>%d</dd>
          <dt>Rejected by conversion</dt><dd>%d</dd>
          <dt>Accepted</dt><dd>%d</dd>
        </dl>
      </dd>
    </dl>
  |}
    !packages_total !packages_rejected !packages_accepted
    !scripts_total !scripts_parsing_errored !scripts_parsing_rejected
    !scripts_conversion_errored !scripts_conversion_rejected !scripts_accepted;
  fpf fmt "<h2>Scenarii</h2>";
  (
    List.iter
      (fun (name, _) ->
         fpf fmt "<div><h3>%s</h3>%a<a href=\"scenario/%s/index.html\">Details</a></div>"
           (Colis_package.Scenario.name_to_string name)
           Colis_common.Report.pp_viz
           (Filename.concat_l ["scenario"; Colis_package.Scenario.name_to_string name; "flowchart.dot"])
           (Colis_package.Scenario.name_to_string name);

         generate_and_write_for_scenario ~prefix name packages)
      Colis_package.Scenarii.all
  );
