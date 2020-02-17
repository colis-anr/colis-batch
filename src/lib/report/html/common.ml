open Colis_batch_ext

type tap = (string * string list) list
(** Title and path. *)

let title_sep = " > "

(* Pathes ALWAYS lead to a file. A tap may however be empty. *)

let root_tap = List.cons ("Report", ["index.html"])

let path_from_tap tap =
  let tap = root_tap tap in
  (
    tap
    |> List.bd           (* For all except the last one: *)
    |> List.map snd      (* Forget about the title. *)
    |> List.map List.bd  (* Keep everything but the last file. *)
    |> List.concat
  ) @ (
    tap
    |> List.ft           (* For the last one: *)
    |> snd               (* Keep everything. *)
  )

let raw_title_from_tap tap =
  tap
  |> root_tap
  |> List.map fst       (* Keep only the title. *)
  |> String.concat title_sep (* Concatenate with the separator. *)

let root_path_from_tap tap =
  tap
  |> root_tap
  |> List.map snd
  |> List.map List.bd
  |> List.concat
  |> List.map (function "." -> "." | ".." -> assert false | _ -> "..")
  |> List.cons "."

let html_title_from_tap tap =
  let root_path = root_path_from_tap tap in
  tap
  |> root_tap
  |> List.fold_left
    (fun (full_title, full_path) (title, path) ->
       ((spf "<a href=\"%s\">%s</a>" (Filename.concat_l (full_path @ path)) title) :: full_title,
         full_path @ (List.bd path)))
    ([], root_path)
  |> fst
  |> List.rev
  |> String.concat title_sep

let pp_header ?(datatables=false) ?(highlight=false) ?(viz=false) tap fmt () =
  let root_path = root_path_from_tap tap |> Filename.concat_l in
  fpf fmt {|
        <!DOCTYPE html>
        <html>
          <head>
            <title>%s</title>

            <meta charset="utf-8" />
            <link rel="stylesheet" type="text/css" href="%s/static/reset.css">
            <link rel="stylesheet" type="text/css" href="%s/static/style.css">
        |}
    (raw_title_from_tap tap) root_path root_path;

  let jquery = datatables in

  if jquery then
    fpf fmt {|
      <script type="text/javascript" src="%s/static/jQuery/3.4.1/jquery-3.4.1.min.js"></script>
    |} root_path;

  if datatables then
    fpf fmt {|
        <link rel="stylesheet" type="text/css" href="%s/static/DataTables/1.10.20/datatables.min.css"/>
        <script type="text/javascript" src="%s/static/DataTables/1.10.20/datatables.min.js"></script>
      |} root_path root_path;

  if highlight then
    fpf fmt {|
            <link rel="stylesheet" href="%s/static/highlight.js/9.14.2/styles/github.min.css">
            <script src="%s/static/highlight.js/9.14.2/highlight.min.js"></script>
            <script src="%s/static/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"></script>
            <script>
              hljs.initHighlightingOnLoad();
              hljs.initLineNumbersOnLoad();
            </script>
          |}
      root_path root_path root_path;

  if viz then
    fpf fmt {|
            <script src="%s/static/viz.js/v2.1.2/viz.js"></script>
            <script src="%s/static/viz.js/v2.1.2/full.render.js"></script>
          |}
      root_path root_path;

  fpf fmt {|
          </head>
          <body>
            <header>
              <div class="content">
                <h1>%s</h1>
              </div>
            </header>
            <div class="content">
        |}
    (html_title_from_tap tap)

let pp_footer fmt () =
  fpf fmt {|
            </div>
            <footer>
              <div class="content">
                <a href="http://colis.irif.fr">
                  ANR Project CoLiS<br/>
                  Correctness of Linux Scripts<br/>
                  ANR-15-CE25-0001<br/>
                  1/10/2015 - 30/9/2020
                </a>
              </div>
            </footer>
          </body>
        </html>
      |}

let with_formatter_to_html_report ?datatables ?highlight ?viz ~prefix (tap : tap) f =
  let path = path_from_tap tap in
  Colis_batch_report_common.with_formatter_to_file ~prefix path @@ fun fmt ->
  pp_header ?datatables ?highlight ?viz tap fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y

type order = Asc | Desc

let pp_datatable ?(order=[]) fmt id =
  fpf fmt {|
      <script>
        $(document).ready( function () {
          $('#%s').DataTable( {
            "order": [%a],
            "paging": false
          } );
        } );
      </script>
    |}
    id
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> fpf fmt ", ")
       (fun fmt (col, ord) -> fpf fmt "[%d, \"%s\"]" col (match ord with Asc -> "asc" | Desc -> "desc")))
    order

let pp_viz fmt file =
  let id = string_of_int (Random.int (1 lsl 29)) in
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

let extract_static ~prefix =
  [
    "DataTables/1.10.20/datatables.min.css", [%blob "static/DataTables/1.10.20/datatables.min.css"];
    "DataTables/1.10.20/datatables.min.js", [%blob "static/DataTables/1.10.20/datatables.min.js"];

    "DataTables/1.10.20/DataTables-1.10.20/images/sort_asc.png", [%blob "static/DataTables/1.10.20/DataTables-1.10.20/images/sort_asc.png"];
    "DataTables/1.10.20/DataTables-1.10.20/images/sort_asc_disabled.png", [%blob "static/DataTables/1.10.20/DataTables-1.10.20/images/sort_asc_disabled.png"];
    "DataTables/1.10.20/DataTables-1.10.20/images/sort_both.png", [%blob "static/DataTables/1.10.20/DataTables-1.10.20/images/sort_both.png"];
    "DataTables/1.10.20/DataTables-1.10.20/images/sort_desc.png", [%blob "static/DataTables/1.10.20/DataTables-1.10.20/images/sort_desc.png"];
    "DataTables/1.10.20/DataTables-1.10.20/images/sort_desc_disabled.png", [%blob "static/DataTables/1.10.20/DataTables-1.10.20/images/sort_desc_disabled.png"];

    "highlight.js/9.14.2/styles/github.min.css", [%blob "static/highlight.js/9.14.2/styles/github.min.css"];
    "highlight.js/9.14.2/highlight.min.js", [%blob "static/highlight.js/9.14.2/highlight.min.js"];
    "highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js", [%blob "static/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"];
    "jQuery/3.4.1/jquery-3.4.1.min.js", [%blob "static/jQuery/3.4.1/jquery-3.4.1.min.js"];
    "viz.js/v2.1.2/full.render.js", [%blob "static/viz.js/v2.1.2/full.render.js"];
    "viz.js/v2.1.2/viz.js", [%blob "static/viz.js/v2.1.2/viz.js"];
    "reset.css", [%blob "static/reset.css"];
    "style.css", [%blob "static/style.css"];
  ]
  |> List.iter
    (fun (path, content) ->
       let path = Filename.concat_l [prefix; "static"; path] in
       let dir = Filename.dirname path in
       Filesystem.mkdir ~parents:true dir;
       Filesystem.write_string_to_file ~content path)

let percentage a b =
  100. *. (significant ~precision:2 ((foi a) /. (foi b)))

let pp_details_list fmt ~text_after ~total cases =
  fpf fmt "<p>Out of a total of <strong>%d</strong> %s" total text_after;
  let cases =
    cases
    |> List.filter (fun c -> fst c <> 0)
    |> List.sort (fun c1 c2 -> compare (fst c1) (fst c2))
  in
  match cases with
  | [] -> failwith "pp_details_list"
  | [nb, text] ->
    fpf fmt ", %d (%g%%) %s.</p>"
      nb (percentage nb total) text
  | (nb, text) :: cases ->
    fpf fmt ":</p><ul>";
    List.iter
      (fun (nb, text) ->
         fpf fmt "<li><strong>%d</strong> (%g%%) %s,</li>"
           nb (percentage nb total) text)
      (List.rev cases);
    fpf fmt "<li>and <strong>%d</strong> (%g%%) %s.</li></ul>"
      nb (percentage nb total) text

let slug str =
  let out = Bytes.create (String.length str) in
  for i = 0 to String.length str - 1 do
    let c = Char.code str.[i] in
    if (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57) then
      Bytes.set out i str.[i]
    else
      Bytes.set out i '-'
  done;
  Bytes.unsafe_to_string out ^ "-" ^ (soi (Hashtbl.hash str))
