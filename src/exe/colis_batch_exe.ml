open Colis_batch

let config = Config.default

let () = assert (not (Sys.file_exists config.report))
let () = assert (Sys.file_exists config.external_sources)

let () =
  Colis.Internals.Options.cpu_time_limit := config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit config.memory_limit;
  Colis.Internals.Options.external_sources := config.external_sources;
  Colis.Internals.Options.fail_on_unknown_utilities := true

let () =
  let (meta, reports) =
    Report.Meta.while_gathering_meta @@ fun () ->
    Sys.argv
    |> Array.to_list
    |> List.tl
    |> List.map (parse_package_from_dir ~content:[])
    |> List.map (analyse_package ~config)
  in
  match reports with
  | [] -> failwith "no input"
  | [report] ->
    generate_html_package_report ~standalone:true ~prefix:config.report report
  | reports ->
    List.iter (generate_html_package_report ~standalone:false ~prefix:config.report) reports;
    let report =
      reports
      |> List.map summarize_package_report
      |> make_batch_report ~meta ~config
    in
    generate_html_batch_report ~prefix:config.report report
