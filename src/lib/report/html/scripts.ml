open Colis_batch_ext
open Colis_batch_report_common.Batch

let pp_utility fmt utility =
  fpf fmt "<p>%s has %d occurrences and a score of %g.</p>"
    utility.name utility.occurrences utility.score;
  fpf fmt "<table><tr><th>Options</th><th>Occurrences</th></tr>";
  List.iter
    (fun (options, nb) ->
       fpf fmt "<tr><td>";
       (match options with
        | [] -> fpf fmt "<i>(no options)</i>"
        | _ -> Format.pp_print_list Format.pp_print_string fmt options);
       fpf fmt "</td><td>%d</td></tr>" nb;
    )
    utility.options;
  fpf fmt "</table>"

let pp_utilities fmt utilities =
  fpf fmt "<p>This table gives an overview of the utilities used in Colis scripts
    (that is only the scripts that were converted). The score gives an idea of the
    number of scripts that would be gained by specifying a utility. Specified
    utilities have thus a score of 0.</p><hr/>";
  fpf fmt "<table id=\"utilities\"><thead><tr><th class=\"align-left\">Name</th><th class=\"align-right\">Occurrences</th><th class=\"align-right\">Score</th></tr></thead><tbody>";
  List.iter
    (fun utility ->
       fpf fmt "<tr><td class=\"align-left\"><a href=\"%s.html\">%s</a></td><td class=\"align-right\">%d</td><td class=\"align-right\">%g</td></tr>"
         (Common.slug utility.name) utility.name utility.occurrences utility.score)
    utilities;
  fpf fmt "</tbody></table>";
  Common.pp_datatable fmt "utilities"
    ~order:[2, Common.Desc]

let generate_utilities ~prefix utilities =
  let tap = ["Utilities", ["utilities"; "index.html"]] in
  (
    Common.with_formatter_to_html_report ~prefix tap ~datatables:true @@ fun fmt ->
    pp_utilities fmt utilities
  );
  List.iter
    (fun utility ->
       Common.with_formatter_to_html_report ~prefix
         (tap @ [utility.name, [Common.slug utility.name ^ ".html"]])
       @@ fun fmt ->
       pp_utility fmt utility)
    utilities

let generate ~prefix scripts =
  generate_utilities ~prefix scripts.utilities

let pp_summary fmt numbers =
  Common.pp_details_list fmt
    ~text_after:"scripts"
    ~total:numbers.total
    [ numbers.parsing_errored, "provoked an error during parsing" ;
      numbers.parsing_rejected, "were rejected by parsing" ;
      numbers.conversion_errored , "provoked an error during conversion" ;
      numbers.conversion_rejected , "were rejected by conversion" ;
      numbers.accepted, "were accepted" ];
  fpf fmt "<p><a href=\"utilities/index.html\">Details about utilities</a></p>"
