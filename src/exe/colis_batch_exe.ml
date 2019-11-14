open Colis_batch

let config = Config.default

let () = assert (not (Sys.file_exists config.report))
let () = assert (Sys.file_exists config.external_sources)
let () = assert (List.for_all Sys.file_exists config.contents)

let () =
  Colis.Internals.Options.cpu_time_limit := config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit config.memory_limit;
  Colis.Internals.Options.external_sources := config.external_sources;
  Colis.Internals.Options.fail_on_unknown_utilities := true

let () =
  Format.eprintf "Reading contents files... @?";
  List.iter Contents.scan config.contents;
  Format.eprintf "done.@."

let () =
  let (meta, reports) =
    Report.Meta.while_gathering_meta @@ fun () ->
    Sys.argv
    |> Array.to_list
    |> List.tl
    |> List.map
      (fun package_path ->
         Format.eprintf "Parsing \"%s\"... @?" package_path;
         let package_name =
           let name_version = Filename.basename package_path in
           let l = String.index name_version '_' in
           String.sub name_version 0 l
         in
         let package = parse_package_from_dir ~content:(Contents.get_files package_name) package_path in
         Format.eprintf "done@.";
         package)
    |> List.mapi
      (fun i package ->
         Format.eprintf "Analysing #%d: %s... @?" (i+1) (Model.Package.name package);
         let report = analyse_package ~config package in
         Format.eprintf "done@.";
         report)
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
