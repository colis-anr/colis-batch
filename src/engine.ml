let pf = Format.printf

let find_packages () =
  !Options.corpus
  |> Sys.readdir
  |> Array.to_list

let handle_package name =
  let report_path = ReportHelpers.package_path ~package:name "index.html" in
  ReportHelpers.with_formatter_to_file report_path @@ fun fmt ->
  HtmlReport.pp_header ~title:name fmt ();
  pf "Package: %s.@." name;
  let package = Package.parse name in
  HtmlReport.Package.pp_parsing_status fmt name;
  (
    match package with
    | None ->
      pf "Parsing failed.@."
    | Some package ->
      pf "Parsing successful.@.";
      List.iter
        (fun (name, scenario) ->
           pf "Scenario: %s.@." (Scenario.name_to_string name);
           let ran = ScenarioEngine.run ~package ~name scenario in
           ignore ran) (* FIXME *)
        Scenarii.all;
      HtmlReport.Package.pp_scenarii fmt (Package.name package)
  );
  pf "@.";
  HtmlReport.pp_footer fmt ()
