open Colis_batch_ext
module Model = Colis_batch_model
open Colis_batch_report_common.Batch

let pp_scenarios_summary fmt (numbers: numbers) =
  Common.pp_details_list fmt
    ~text_after:(spf "scenarios (%d packages × %d scenarios)" numbers.packages numbers.scenarios.per_package)
    ~total:numbers.scenarios.stotal
    [ numbers.scenarios.complete, "were ran completely" ;
      numbers.scenarios.partial, "were ran partially" ;
      numbers.scenarios.failure, "could not be run at all" ];

  Common.pp_details_list fmt
    ~text_after:"problems"
    ~total:numbers.scenarios.problems
    [ numbers.scenarios.not_converted, "scripts not converted" ;
      numbers.scenarios.timeout, "timeouts" ;
      numbers.scenarios.out_of_memory, "out of memory" ;
      numbers.scenarios.incomplete, "incompletness" ;
      numbers.scenarios.unsupported_utility, "unsupported utilities (<a href=\"unsupported-utilities.html\">details</a>)" ;
      numbers.scenarios.unexpected, "unexpected exceptions" ]

let pp_index fmt report =
  fpf fmt "<h2>Configuration</h2>";
  Config.pp fmt report.config;
  fpf fmt "<h2>Meta</h2>";
  Meta.pp fmt report.meta;
  fpf fmt "<h2>Scripts</h2>";
  Scripts.pp_summary fmt report.numbers.scripts;
  fpf fmt "<h2>Scenarios</h2>";
  pp_scenarios_summary fmt report.numbers;
  List.iter
    (fun (name, _scenario) ->
       fpf fmt "<h3>%s</h3><div>"
         (Model.Scenarii.Name.to_fancy_string name);
       fpf fmt "<div>%a</div>"
         Common.pp_viz
         (Filename.concat_l ["scenario"; Model.Scenarii.Name.to_string name; "flowchart.dot"]);

       fpf fmt "<a href=\"scenario/%s/index.html\">Details</a></div>"
         (Model.Scenarii.Name.to_string name))
    Model.Scenarii.all

(* FIXME: a special page for scripts that haven't been converted and why
   (parsing error, conversion, rejection, etc.). This page should also list the
   unsupported utilities in scripts. *)

let pp_scenario fmt report scenario =
  fpf fmt "<div style=\"margin: auto;\">%a</div>"
    Common.pp_viz "flowchart.dot";
  let all_status = Model.Scenario.all_status scenario in
  let package_by_status =
    let package_by_status =
      List.map (fun status -> (status, ref [])) all_status
    in
    List.iter
      (fun package ->
         List.iter
           (fun (_name, scenario) ->
              List.iter
                (fun (status, states) ->
                   if states <> 0 then
                     match List.assoc_opt status package_by_status with
                     | None -> ()
                     | Some others -> others := package :: !others)
              (Model.Scenario.states_sum scenario)
           )
           package.Colis_batch_report_common.Package.scenarii
      )
      report.packages;
    List.map
      (fun (status, packages) ->
         (status,
          List.sort_uniq
            (fun p1 p2 ->
               compare
                 (Model.Package.name p1.Colis_batch_report_common.Package.package)
                 (Model.Package.name p2.Colis_batch_report_common.Package.package))
            !packages))
      package_by_status
  in
  fpf fmt "<h2>Summary</h2><ul>";
  List.iter
    (fun (status, packages) ->
       fpf fmt "<li><a href=\"#%a\">%a</a> (%d)</li>"
         Model.Scenario.Status.pp status
         Model.Scenario.Status.pp status
         (List.length packages))
    package_by_status;
  fpf fmt "</ul>";
  List.iter
    (fun (status, packages) ->
       fpf fmt "<h2 id=\"%a\">%a</h2><ul>"
         Model.Scenario.Status.pp status
         Model.Scenario.Status.pp status;
       List.iter
         (fun package ->
            fpf fmt "<li><a href=\"../../package/%s/index.html\">%s</a></li>"
              (Model.Package.safe_name package.Colis_batch_report_common.Package.package)
              (Model.Package.name package.Colis_batch_report_common.Package.package)
         )
         packages;
       fpf fmt "</ul>")
    package_by_status

let generate_scenario ~prefix report name scenario =
  (
    Colis_batch_report_common.with_formatter_to_file ~prefix
      ["scenario"; Model.Scenarii.Name.to_string name; "flowchart.dot"]
    @@ fun fmt ->
    Model.Scenario.pp_clean_as_dot fmt scenario
  );
  (
    Common.with_formatter_to_html_report
      ~viz:true ~prefix
      [Model.Scenarii.Name.to_fancy_string name,
       ["scenario"; Model.Scenarii.Name.to_string name; "index.html"]]
    @@ fun fmt ->
    pp_scenario fmt report scenario
  )

let generate ~prefix report =
  Common.extract_static ~prefix;
  ( (* Index. *)
    Common.with_formatter_to_html_report ~viz:true ~prefix [] @@ fun fmt ->
    pp_index fmt report
  );
  Scripts.generate ~prefix report.scripts;
  List.iter
    (fun (name, scenario) ->
       generate_scenario ~prefix report name scenario)
    Model.Scenarii.all
