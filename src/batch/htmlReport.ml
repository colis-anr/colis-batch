open Colis_ext

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

let gaw_packages ~prefix packages =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Packages"
    [prefix; "packages.html"]
  @@ fun fmt ->
  pp_packages fmt packages

let gaw_packages_accepted ~prefix packages =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Packages Accepted by Parsing"
    [prefix; "parsing"; "packages-accepted.html"]
  @@ fun fmt ->
  pp_packages ~anti_prefix:".." fmt packages

let gaw_packages_rejected ~prefix packages =
  let packages = List.filter (fun pkg -> not (Colis_package.Package.are_all_maintscripts_ok pkg)) packages in
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Packages Rejected by Parsing"
    [prefix; "parsing"; "packages-rejected.html"]
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

let gaw_scripts ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts"
    [prefix; "scripts.html"]
  @@ fun fmt ->
  pp_scripts fmt scripts

let gaw_scripts_parsing_rejected ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts Rejected by Parsing"
    [prefix; "parsing"; "scripts-parsing-rejected.html"]
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

let gaw_scripts_parsing_errored ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts with Error in Parsing"
    [prefix; "parsing"; "scripts-parsing-errored.html"]
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

let gaw_scripts_conversion_rejected ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts Rejected by Conversion"
    [prefix; "parsing"; "scripts-conversion-rejected.html"]
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

let gaw_scripts_conversion_errored ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts with Error in Conversion"
    [prefix; "parsing"; "scripts-conversion-errored.html"]
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

let gaw_scripts_accepted ~prefix scripts =
  Colis_common.Report.with_formatter_to_html_report
    ~title:"Scripts Accepted by Parsing"
    [prefix; "parsing"; "scripts-accepted.html"]
  @@ fun fmt ->
  pp_scripts ~anti_prefix:".." fmt scripts

let generate_and_write_for_scenario ~prefix name packages_and_scenarii =
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
       fpf fmt "<li><a href=\"#%a\">%a</a> (%d)</li>"
         Colis_package.Scenario.Status.pp status
         Colis_package.Scenario.Status.pp status
         (
           let r = ref 0 in
           List.iter
             (fun (_package, scenarii) ->
                List.iter
                  (fun (name', states) ->
                     if name' = name then
                       (
                         List.iter
                           (fun (status', states) ->
                              if status' = status && states <> 0 then
                                incr r
                           )
                           states
                       )
                  )
                  scenarii
             )
             packages_and_scenarii;
           !r
         )
    )
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
              (fun (name', states) ->
                 if name' = name then
                   (
                     List.iter
                       (fun (status', states) ->
                          if status' = status && states <> 0 then
                            fpf fmt "<li><a href=\"../../package/%s/index.html\">%s</a></li>"
                              (Colis_package.Package.safe_name package)
                              (Colis_package.Package.safe_name package)
                       )
                       states
                   )
              )
              scenarii
         )
         packages_and_scenarii
    )
    all_status

let gaw_pp_parsing fmt ~prefix packages_and_scenarii =
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

  gaw_packages ~prefix packages;
  gaw_packages_accepted ~prefix packages_accepted;
  gaw_packages_rejected ~prefix packages_rejected;

  gaw_scripts ~prefix scripts;
  gaw_scripts_parsing_rejected ~prefix scripts_parsing_rejected;
  gaw_scripts_parsing_errored ~prefix scripts_parsing_errored;
  gaw_scripts_conversion_rejected ~prefix scripts_conversion_rejected;
  gaw_scripts_conversion_errored ~prefix scripts_conversion_errored;
  gaw_scripts_accepted ~prefix scripts_accepted

let generate_and_write ~prefix ~time packages_and_scenarii =

  Colis_common.Report.with_formatter_to_html_report
    ~title:"Report"
    ~viz:true
    [prefix; "index.html"]
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
      <dt>Total time</dt><dd>%.0fs</dd>
    </dl>
  |}
    (floor (0.5 +. time));

  gaw_pp_parsing fmt ~prefix packages_and_scenarii;

  fpf fmt "<h2>Scenarii</h2>";
  (
    List.iter
      (fun (name, _) ->
         fpf fmt "<div><h3>%s</h3>%a<a href=\"scenario/%s/index.html\">Details</a></div>"
           (Colis_package.Scenario.name_to_fancy_string name)
           Colis_common.Report.pp_viz
           (Filename.concat_l ["scenario"; Colis_package.Scenario.name_to_string name; "flowchart.dot"])
           (Colis_package.Scenario.name_to_string name);

         generate_and_write_for_scenario ~prefix name packages_and_scenarii)
      Colis_package.Scenarii.all
  );
