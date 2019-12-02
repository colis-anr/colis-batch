open Colis_batch open Colis_batch_ext

let config = !Config.config

let () =
  Colis.Internals.Options.cpu_time_limit := config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit config.memory_limit;
  Colis.Internals.Options.external_sources := config.external_sources;
  Colis.Internals.Options.fail_on_unknown_utilities := true

let () =
  if config.contents <> "" then
    (
      Format.eprintf "Reading contents files... @?";
      Contents.scan config.contents;
      Format.eprintf "done.@."
    )

let package_pathes =
  if config.corpus <> "" then
    (
      config.corpus
      |> Sys.readdir
      |> Array.to_list
      |> List.sort compare
      |> List.map (Filename.concat config.corpus)
    )
  else
    (
      assert (config.package <> "");
      [config.package]
    )

let () =
  let (meta, reports) =
    Report.Meta.while_gathering_meta @@ fun () ->
    MultiProcess.map_p
      ~workers:config.workers
      (fun package_path ->
         Format.eprintf "%s@." package_path;
         let package_name =
           let name_version = Filename.basename package_path in
           let l = String.index name_version '_' in
           String.sub name_version 0 l
         in
         let package = parse_package_from_dir ~content:(Contents.get_files package_name) package_path in
         let report = analyse_package ~config package in
         report)
      package_pathes
    |> Lwt_main.run
  in
  match reports with
  | [] -> failwith "no input"
  | [report] ->
    generate_html_package_report ~standalone:true ~prefix:config.report report
  | reports ->
    MultiProcess.iter_p
      ~workers:config.workers
      (generate_html_package_report ~standalone:false ~prefix:config.report) reports
    |> Lwt_main.run;
    let report =
      reports
      |> List.map summarize_package_report
      |> make_batch_report ~meta ~config
    in
    generate_html_batch_report ~prefix:config.report report
