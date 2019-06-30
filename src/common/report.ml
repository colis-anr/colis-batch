open Colis_ext

let copy_static_to path =
  assert (0 = Sys.command (spf "cp -R %S/static %S/%S"
                             !Config.share
                             !Config.report (String.concat "/" path)))

let pp_header ~title ?(highlight=false) ?(viz=false) ~rev_path fmt () =
  fpf fmt {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>%s</title>

        <meta charset="utf-8" />
        <link rel="stylesheet" type="text/css" href="%s/static/reset.css">
        <link rel="stylesheet" type="text/css" href="%s/static/style.css">
    |}
    title rev_path rev_path;
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
      rev_path rev_path rev_path;
  if viz then
    fpf fmt {|
        <script src="%s/static/viz.js/v2.1.2/viz.js"></script>
        <script src="%s/static/viz.js/v2.1.2/full.render.js"></script>
      |}
      rev_path rev_path;
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
    title

let pp_footer fmt () =
  fpf fmt {|
        </div>
        <footer>
          <div class="content">
            <a href="http://colis.irif.fr">
              ANR Project CoLiS<br/>
              Correctness of Linux Scripts<br/>
              ANR-15-CE25-0001,1/10/2015 - 30/9/2020
            </a>
          </div>
        </footer>
      </body>
    </html>
  |}

let ensure_existence path =
  if Sys.command (spf "mkdir -p %S" path) <> 0 then
    failwith "ensure_existence"

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

let with_formatter_to_file path f =
  let path = Filename.(concat !Config.report (concat_l path)) in
  ensure_existence (Filename.dirname path);
  let ochan = open_out path in
  let fmt = Format.formatter_of_out_channel ochan in
  let y = f fmt in
  Format.pp_print_flush fmt ();
  close_out ochan;
  y

let with_formatter_to_html_report ~title ?highlight ?viz path f =
  let rev_path = path |> List.tl |> List.map (fun _ -> "..") |> List.cons "." |> String.concat "/" in
  with_formatter_to_file path @@ fun fmt ->
  pp_header ~title ?highlight ?viz ~rev_path fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y
