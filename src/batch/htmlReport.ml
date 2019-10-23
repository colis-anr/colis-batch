open Colis_ext

let percentage a b = 100 * a / b (* FIXME *)

let pp_packages ?(anti_prefix=".") fmt packages =
  fpf fmt "<ul>";
  List.iter
    (fun package ->
       fpf fmt "<li><a href=\"%s/package/%s/index.html\">%s</a></li>"
         anti_prefix
         (Colis_package.Package.safe_name package)
         (Colis_package.Package.name package))
    packages;
  fpf fmt "</ul>"

let gaw_packages packages =
  Colis_common.Report.with_formatter_to_html_report
    ["Packages", ["packages.html"]]
  @@ fun fmt ->
  pp_packages fmt packages

let gaw_packages_accepted packages =
  Colis_common.Report.with_formatter_to_html_report
    ["Packages Accepted by Parsing", ["parsing"; "packages-accepted.html"]]
  @@ fun fmt ->
  pp_packages ~anti_prefix:".." fmt packages

let gaw_packages_rejected packages =
  let packages = List.filter (fun pkg -> not (Colis_package.Package.are_all_maintscripts_ok pkg)) packages in
  Colis_common.Report.with_formatter_to_html_report
    ["Packages Rejected by Parsing", ["parsing"; "packages-rejected.html"]]
  @@ fun fmt ->
  pp_packages ~anti_prefix:".." fmt packages

let pp_scripts ?(anti_prefix=".") fmt scripts =
  fpf fmt "<ul>";
  List.iter
    (fun (package, script) ->
       let name = Colis_package.Maintscript.key_as_string script in
       fpf fmt "<li><a href=\"%s/package/%s/script/%s.html\">%s &gt; %s</a></li>"
         anti_prefix
         (Colis_package.Package.safe_name package)
         name
         (Colis_package.Package.name package)
         name)
    scripts;
  fpf fmt "</ul>"

let gaw_scripts scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts", ["scripts.html"]]
  @@ fun fmt ->
  pp_scripts fmt scripts

let gaw_scripts_parsing_rejected scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts Rejected by Parsing", ["parsing"; "scripts-parsing-rejected.html"]]
  @@ fun fmt ->
  pp_scripts ~anti_prefix:".." fmt scripts

let group_scripts_by_error extract_error scripts =
  scripts
  |> List.map (fun (package, script) -> (extract_error script, (package, script)))
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.group compare
  |> List.sort (fun (_, l1) (_, l2) -> - List.compare_lengths l1 l2)

let pp_scripts_by_error fmt extract_error scripts =
  let scripts_by_error = group_scripts_by_error extract_error scripts in
  fpf fmt "<ul>";
  List.iter
    (fun (msg, scripts) ->
       fpf fmt "<li>(%d) <a href=\"#%d\">%s</a></li>"
         (List.length scripts)
         (Hashtbl.hash msg)
         msg)
    scripts_by_error;
  fpf fmt "</ul>";
  List.iter
    (fun (msg, scripts) ->
       fpf fmt "<h3 id=\"%d\">%s</h3>" (Hashtbl.hash msg) msg;
       pp_scripts ~anti_prefix:".." fmt scripts)
    scripts_by_error

let gaw_scripts_parsing_errored scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts with Error in Parsing", ["parsing"; "scripts-parsing-errored.html"]]
  @@ fun fmt ->
  fpf fmt "<h2>By Error</h2>";
  pp_scripts_by_error
    fmt
    (fun script ->
       match Colis_package.Maintscript.error script with
       | Some (ParsingErrored msg) -> msg
       | _ -> assert false)
    scripts;
  fpf fmt "<h2>All</h2>";
  pp_scripts ~anti_prefix:".." fmt scripts

let gaw_scripts_conversion_rejected scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts Rejected by Conversion", ["parsing"; "scripts-conversion-rejected.html"]]
  @@ fun fmt ->
  fpf fmt "<h2>By Error</h2>";
  pp_scripts_by_error
    fmt
    (fun script ->
       match Colis_package.Maintscript.error script with
       | Some (ConversionRejected msg) -> msg
       | _ -> assert false)
    scripts;
  fpf fmt "<h2>All</h2>";
  pp_scripts ~anti_prefix:".." fmt scripts

let gaw_scripts_conversion_errored scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts with Error in Conversion", ["parsing"; "scripts-conversion-errored.html"]]
  @@ fun fmt ->
  fpf fmt "<h2>By Error</h2>";
  pp_scripts_by_error
    fmt
    (fun script ->
       match Colis_package.Maintscript.error script with
       | Some (ConversionErrored msg) -> msg
       | _ -> assert false)
    scripts;
  fpf fmt "<h2>All</h2>";
  pp_scripts ~anti_prefix:".." fmt scripts

let gaw_scripts_accepted scripts =
  Colis_common.Report.with_formatter_to_html_report
    ["Scripts Accepted by Parsing", ["parsing"; "scripts-accepted.html"]]
  @@ fun fmt ->
  pp_scripts ~anti_prefix:".." fmt scripts

let pp_summary fmt ?(prefix="") name packages_and_scenarii all_status =
  List.iter
    (fun status ->
       fpf fmt "<li><a href=\"%s#%a\">%a</a> (%d)</li>"
         prefix
         Colis_package.Scenario.Status.pp status
         Colis_package.Scenario.Status.pp status
         (
           let r = ref 0 in
           List.iter
             (fun (_package, scenarii) ->
                List.iter
                  (fun (name', scenario) ->
                     if name' = name then
                       (
                         List.iter
                           (fun (status', states) ->
                              if status' = status && states <> 0 then
                                incr r
                           )
                           (Colis_package.Scenario.states_sum scenario)
                       )
                  )
                  scenarii
             )
             packages_and_scenarii;
           !r
         )
    )
    all_status

let generate_and_write_for_scenario name packages_and_scenarii =
  let scenario = List.assoc name Colis_package.Scenarii.all in
  (
    Colis_common.Report.with_formatter_to_file
      ["scenario"; Colis_package.Scenarii.Name.to_string name; "flowchart.dot"]
    @@ fun fmt ->
    Colis_package.Scenario.pp_clean_as_dot fmt scenario
  );
  Colis_common.Report.with_formatter_to_html_report
    ~viz:true
    [Colis_package.Scenarii.Name.to_fancy_string name, ["scenario"; Colis_package.Scenarii.Name.to_string name]]
  @@ fun fmt ->
  fpf fmt "<div style=\"margin: auto;\">%a</div>"
    Colis_common.Report.pp_viz "flowchart.dot";
  let all_status = Colis_package.Scenario.all_status scenario in
  fpf fmt "<h2>Summary</h2><ul>";
  pp_summary fmt name packages_and_scenarii all_status;
  fpf fmt "</ul>";
  List.iter
    (fun status ->
       fpf fmt "<h2 id=\"%a\">%a</h2><ul>"
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
                     List.iter
                       (fun (status', states) ->
                          if status' = status && states <> 0 then
                            fpf fmt "<li><a href=\"../../package/%s/index.html\">%s</a></li>"
                              (Colis_package.Package.safe_name package)
                              (Colis_package.Package.name package)
                       )
                       (Colis_package.Scenario.states_sum scenario)
                   )
              )
              scenarii
         )
         packages_and_scenarii;
       fpf fmt "</ul>"
    )
    all_status

let gaw_pp_parsing fmt packages_and_scenarii =
  let packages = List.map fst packages_and_scenarii in
  let (packages_accepted, packages_rejected) =
    List.partition Colis_package.Package.are_all_maintscripts_ok packages
  in

  let scripts =
    List.flat_map
      (fun package ->
         List.map
           (fun script -> (package, script))
           (Colis_package.Package.maintscripts package))
      packages
  in
  let scripts_parsing_errored = ref [] in
  let scripts_parsing_rejected = ref [] in
  let scripts_conversion_errored = ref [] in
  let scripts_conversion_rejected = ref [] in
  let scripts_accepted = ref [] in
  let add_to x l = l := x :: !l in
  List.iter
    (fun package_script ->
       let (_, script) = package_script in
       add_to package_script
         (match Colis_package.Maintscript.error script with
          | None -> scripts_accepted
          | Some (ParsingErrored _) -> scripts_parsing_errored
          | Some (ParsingRejected) -> scripts_parsing_rejected
          | Some (ConversionErrored _) -> scripts_conversion_errored
          | Some (ConversionRejected _) -> scripts_conversion_rejected))
    scripts;
  let scripts_parsing_errored = List.rev !scripts_parsing_errored in
  let scripts_parsing_rejected = List.rev !scripts_parsing_rejected in
  let scripts_conversion_errored = List.rev !scripts_conversion_errored in
  let scripts_conversion_rejected = List.rev !scripts_conversion_rejected in
  let scripts_accepted = List.rev !scripts_accepted in

  fpf fmt {|
  <h2>Parsing</h2>

  <h3>Packages</h3>
  <dl>
    <dt><a href="packages.html">Total</a></dt><dd>%d</dd>
    <dt><a href="parsing/packages-rejected.html">Rejected</a></dt><dd>%d</dd>
    <dt><a href="parsing/packages-accepted.html">Accepted</a></dt><dd>%d</dd>
  </dl>

  <h3>Scripts</h3>
  <dl>
    <dt><a href="scripts.html">Total</a></dt><dd>%d</dd>
    <dt><a href="parsing/scripts-parsing-errored.html">Errored in parsing</a></dt><dd>%d</dd>
    <dt><a href="parsing/scripts-parsing-rejected.html">Rejected by parsing</a></dt><dd>%d</dd>
    <dt><a href="parsing/scripts-conversion-errored.html">Errored in conversion</a></dt><dd>%d</dd>
    <dt><a href="parsing/scripts-conversion-rejected.html">Rejected by conversion</a></dt><dd>%d</dd>
    <dt><a href="parsing/scripts-accepted.html">Accepted</a></dt><dd>%d</dd>
  </dl>
|}
    (List.length packages) (List.length packages_rejected) (List.length packages_accepted)
    (List.length scripts) (List.length scripts_parsing_errored) (List.length scripts_parsing_rejected)
    (List.length scripts_conversion_errored) (List.length scripts_conversion_rejected) (List.length scripts_accepted);

  gaw_packages packages;
  gaw_packages_accepted packages_accepted;
  gaw_packages_rejected packages_rejected;

  gaw_scripts scripts;
  gaw_scripts_parsing_rejected scripts_parsing_rejected;
  gaw_scripts_parsing_errored scripts_parsing_errored;
  gaw_scripts_conversion_rejected scripts_conversion_rejected;
  gaw_scripts_conversion_errored scripts_conversion_errored;
  gaw_scripts_accepted scripts_accepted

let generate_and_write ~start_time ~end_time packages_and_scenarii =
  Colis_common.Report.copy_static_to ["static"];

  Colis_common.Report.with_formatter_to_html_report
    ~viz:true
    []
  @@ fun fmt ->

  fpf fmt {|
    <h2>Configuration</h2>
    <dl>
      <dt>Workers</dt><dd>%d</dd>
      <dt>CPU Timeout</dt><dd>%Fs</dd>
    </dl>
  |}
    !Colis_common.Config.workers
    !Colis_common.Config.cpu_timeout;

  fpf fmt {|
    <h2>Meta</h2>
    <dl>
      <dt>Start time</dt><dd>%a</dd>
      <dt>End time</dt><dd>%a</dd>
      <dt>Duration</dt><dd>%.0fs</dd>
    </dl>
  |}
    Unix.pp_time start_time
    Unix.pp_time end_time
    (floor (0.5 +. end_time -. start_time));

  gaw_pp_parsing fmt packages_and_scenarii;

  fpf fmt "<h2>Scenarios</h2>";

  (
    let nb_packages = List.length packages_and_scenarii in
    let nb_scenarii = nb_packages * List.length Colis_package.Scenarii.all in
    let nb_complete = ref 0 in
    let nb_partial = ref 0 in
    let nb_incomplete = ref 0 in
    let nb_timeout = ref 0 in
    let nb_oomemory = ref 0 in
    let nb_notconverted = ref 0 in
    let nb_unsupported = ref 0 in
    let nb_unexpected = ref 0 in
    List.iter
      (fun (_package, scenarii) ->
         List.iter
           (fun (_name, scenario) ->
              let open Colis_package.Scenario in
              match coverage scenario with
              | Null r ->
                if ran_node_incomplete r then incr nb_incomplete;
                if ran_node_timeout r then incr nb_timeout;
                if ran_node_oomemory r then incr nb_oomemory;
                if ran_node_notconverted r then incr nb_notconverted;
                if ran_node_unsupported r then incr nb_unsupported;
                if ran_node_unexpected r then incr nb_unexpected
              | Partial r ->
                incr nb_partial;
                if ran_node_incomplete r then incr nb_incomplete;
                if ran_node_timeout r then incr nb_timeout;
                if ran_node_oomemory r then incr nb_oomemory;
                if ran_node_notconverted r then incr nb_notconverted;
                if ran_node_unsupported r then incr nb_unsupported;
                if ran_node_unexpected r then incr nb_unexpected
              | Complete -> incr nb_complete)
           scenarii)
      packages_and_scenarii;
    let nb_problems =
      !nb_incomplete + !nb_timeout + !nb_oomemory
      + !nb_notconverted + !nb_unsupported + !nb_unexpected
    in

    fpf fmt "<p>";
    fpf fmt "In total, I attempted to run %d scenarios. (%d packages × %d scenarios). "
      nb_scenarii nb_packages (List.length Colis_package.Scenarii.all);
    fpf fmt "I managed to run %d scenarios (%d%%) completely and %d (%d%%) partially. "
      !nb_complete (percentage !nb_complete nb_scenarii)
      !nb_partial (percentage !nb_partial nb_scenarii);
    fpf fmt "I counted %d problems: %d scripts not converted (%d%% of all problems), %d timeouts (%d%%), %d out of memory (%d%%), %d incompletness (%d%%), %d unsupported utilities (%d%%) and %d unexpected exceptions (%d%%). "
      nb_problems
      !nb_notconverted (percentage !nb_notconverted nb_problems)
      !nb_timeout (percentage !nb_timeout nb_problems)
      !nb_oomemory (percentage !nb_oomemory nb_problems)
      !nb_incomplete (percentage !nb_incomplete nb_problems)
      !nb_unsupported (percentage !nb_unsupported nb_problems)
      !nb_unexpected (percentage !nb_unexpected nb_problems);
    fpf fmt "</p>"
  );

  (
    List.iter
      (fun (name, scenario) ->
         fpf fmt "<div style=\"clear: left;\"><h3>%s</h3>"
           (Colis_package.Scenarii.Name.to_fancy_string name);
         fpf fmt "<div style=\"float: left;\">%a</div>"
           Colis_common.Report.pp_viz
           (Filename.concat_l ["scenario"; Colis_package.Scenarii.Name.to_string name; "flowchart.dot"]);
         pp_summary fmt
           ~prefix:(Filename.concat_l ["scenario"; Colis_package.Scenarii.Name.to_string name])
           name packages_and_scenarii (Colis_package.Scenario.all_status scenario);
         fpf fmt "<a href=\"scenario/%s/index.html\">Details</a></div>"
           (Colis_package.Scenarii.Name.to_string name);

         generate_and_write_for_scenario name packages_and_scenarii)
      Colis_package.Scenarii.all
  );
