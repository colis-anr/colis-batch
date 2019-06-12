open Colis_ext

let generate_and_write_for_scenario ~prefix name _scenarii =
  (
    let scenario = List.assoc name Colis_package.Scenarii.all in
    Colis_common.Report.with_formatter_to_file
      [prefix; "scenario"; Colis_package.Scenario.name_to_string name; "flowchart.dot"]
    @@ fun fmt ->
    Colis_package.Scenario.pp_unit_as_dot ~name fmt scenario
  );
  Colis_common.Report.with_formatter_to_html_report
    ~title:"CoLiS – Scenario Report"
    ~viz:true
    [prefix; "scenario"; Colis_package.Scenario.name_to_string name; "index.html"]
  @@ fun fmt ->
  Colis_common.Report.pp_viz fmt "flowchart.dot"

let generate_and_write ~prefix ~time packages =
  (
    List.iter
      (fun (name, _) ->
         generate_and_write_for_scenario ~prefix name ())
      Colis_package.Scenarii.all
  );
  Colis_common.Report.with_formatter_to_html_report
    ~title:"CoLiS – Report"
    [prefix; "index.html"]
  @@ fun fmt ->
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
    <dl>
      <dt>Packages</dt>
      <dd>
        <dl>
          <dt>Total</dt><dd>%d</dd>
          <dt>Rejected</dt><dd>%d</dd>
          <dt>Accepted</dt><dd>%d</dd>
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

      <dt>Total time</dt><dd>%.0fs</dd>
    </dl>
  |}
    !packages_total !packages_rejected !packages_accepted
    !scripts_total !scripts_parsing_errored !scripts_parsing_rejected
    !scripts_conversion_errored !scripts_conversion_rejected !scripts_accepted
    (floor (0.5 +. time))
