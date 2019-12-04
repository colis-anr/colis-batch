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
    utility.options

let pp_utilities fmt utilities =
  fpf fmt "<table><tr><th>Utility</th><th>Occurrences</th><th>Score</th></tr>";
  List.iter
    (fun utility ->
       fpf fmt "<tr><td>%s</td><td>%d</td><td>%g</td></tr>"
         utility.name utility.occurrences utility.score)
    utilities;
  fpf fmt "</table>"

let generate_utilities ~prefix utilities =
  let tap = ["Utilities", ["utilities"; "index.html"]] in
  (
    Common.with_formatter_to_html_report ~prefix tap @@ fun fmt ->
    pp_utilities fmt utilities
  );
  List.iter
    (fun utility ->
       Common.with_formatter_to_html_report ~prefix
         (tap @ [utility.name, [utility.name ^ ".html"]])
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
      numbers.accepted, "were accepted" ]
