open Colis_batch open Colis_batch_ext

let pad_int_left_to y x = (*FIXME: move to ext?*)
  let len x = iof (ceil (log10 (foi x +. 1.))) in
  spf "%s%d" (String.make (max 0 (len y - len x)) ' ') x

let config = !Config.config

let (keys, nb_keys) =
  epf "Reading cache content... @?";
  let keys = Sys.readdir config.cache in
  let nb_keys = Array.length files in
  epf "done. Found %d files.@." nb_keys;
  (keys, nb_keys)

let summaries =
  epf "Generating package reports... @.";
  let summaries =
    keys
    |> Array.to_list
    |> MultiProcess.mapi_p ~workers:config.workers (fun i key ->
        let i = i + 1 in
        epf "\r  [%s/%d; %3d%%] %s   @?" (pad_int_left_to nb_keys i) nb_keys (100 * i / nb_keys) key;
        key
        |> load_package_report_as_bin ~cache:config.cache
        |> generate_html_package_report ~standalone:false ~prefix:config.report)
  in
  epf "done.@.";
  summaries

let batch =
  epf "Combining package reports into a batch report... @?";
  let batch =
    summaries
    |> make_batch_report ~meta ~config
    |> enrich_batch_report
  in
  epf "done.@.";
  batch

let () =
  epf "Generating batch report... @?";
  generate_html_batch_report ~prefix:config.report report;
  epf "done.@."
