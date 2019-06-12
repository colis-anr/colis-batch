open Colis_ext

let pp_header ~title ?(highlight=false) ?(viz=false) fmt () =
  fpf fmt {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>%s</title>

        <meta charset="utf-8" />
        <style type="text/css">
          body { width: 80%%; margin: auto; }
          .hidden { display: none; }
          .accepted { background: #afa; }
          .rejected { background: #aaf; }
          .errored  { background: #faa; }
          .empty    { background: #ddd; }
          pre { max-width: 100%%; overflow: auto; }
        </style>
    |}
    title;
  if highlight then
    fpf fmt {|
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/styles/github.min.css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.14.2/highlight.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/highlightjs-line-numbers.js/2.6.0/highlightjs-line-numbers.min.js"></script>
        <script>
          hljs.initHighlightingOnLoad();
          hljs.initLineNumbersOnLoad();
        </script>
      |};
  if viz then
    fpf fmt {|
        <script src="https://github.com/mdaines/viz.js/releases/download/v2.1.2/viz.js"></script>
        <script src="https://github.com/mdaines/viz.js/releases/download/v2.1.2/full.render.js"></script>
      |};
  fpf fmt {|
      </head>
      <body>
        <h1>%s</h1>
    |}
    title

let pp_footer fmt () =
  fpf fmt "</body></html>"

let rec ensure_existence path =
  let dir = Filename.dirname path in
  if dir <> path then ensure_existence dir;
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let with_formatter_to_file path f =
  let path = Filename.concat_l path in
  ensure_existence (Filename.dirname path);
  let ochan = open_out path in
  let fmt = Format.formatter_of_out_channel ochan in
  let y = f fmt in
  Format.pp_print_flush fmt ();
  close_out ochan;
  y

let with_formatter_to_html_report ?title ?highlight ?viz path f =
  let title =
    match title with
    | None -> "CoLiS"
    | Some title -> "CoLiS – " ^ title
  in
  with_formatter_to_file path @@ fun fmt ->
  pp_header ~title ?highlight ?viz fmt ();
  let y = f fmt in
  pp_footer fmt ();
  y
