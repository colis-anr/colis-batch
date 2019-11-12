open Colis_batch_ext

type tap = (string * string list) list
(** Title and path. *)

let title_sep = " > "

(* Pathes ALWAYS lead to a file. *)

let path_from_tap tap =
  tap
  |> List.map snd      (* Forget about the title. *)
  |> List.map List.bd  (* Keep everything but the last file. *)
  |> List.concat

let raw_title_from_tap tap =
  tap
  |> List.map fst       (* Keep only the title. *)
  |> List.cons "Report" (* Add Report as first. *)
  |> String.concat title_sep (* Concatenate with the separator. *)

let root_path_from_tap tap =
  tap
  |> List.map snd
  |> List.map List.bd
  |> List.concat
  |> List.map (function "." -> "." | ".." -> assert false | _ -> "..")

let html_title_from_tap tap =
  let root_path = root_path_from_tap tap in
  tap
  |> List.cons ("Report", ["index.html"])
  |> List.fold_left
    (fun (full_title, full_path) (title, path) ->
       ((spf "<a href=\"%s\">%s</a>" (Filename.concat_l (full_path @ path)) title) :: full_title,
         full_path @ (List.bd path)))
    ([], root_path)
  |> fst
  |> List.rev
  |> String.concat title_sep

let pp_header ?(highlight=false) ?(viz=false) tap fmt () =
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

let with_formatter_to_html_report ?highlight ?viz ~prefix (tap : tap) f =
  let path = path_from_tap tap in
  Colis_batch_report_common.with_formatter_to_file ~prefix path @@ fun fmt ->
  pp_header ?highlight ?viz tap fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y

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
    "highlight.js/9.14.2/styles/github.min.css", [%blob "static/highlight.js/9.14.2/styles/github.min.css"];
    "highlight.js/9.14.2/highlight.min.js", [%blob "static/highlight.js/9.14.2/highlight.min.js"];
    "highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js", [%blob "static/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"];
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
       Filesystem.write_to_file ~content path)
