open Colis_batch_ext

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
