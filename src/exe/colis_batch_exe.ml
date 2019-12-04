open Colis_batch open Colis_batch_ext

let config = !Config.config

let () =
  Colis.Internals.Options.cpu_time_limit := config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit config.memory_limit;
  Colis.Internals.Options.external_sources := config.external_sources;
  Colis.Internals.Options.fail_on_unknown_utilities := true

(* =============================== [ Cache ] ================================ *)

let save_to_cache (report : Report.Package.t) =
  save_package_report_as_bin ~cache:config.cache report

let load_from_cache key : Report.Package.t =
  load_package_report_as_bin ~cache:config.cache key

let cache = Hashtbl.create 8
let () =
  if config.cache <> "" then
    (
      Format.eprintf "Checking cache... @?";
      if not Sys.(file_exists config.cache && is_directory config.cache) then
        Filesystem.mkdir config.cache;
      let files = Sys.readdir config.cache in
      Format.eprintf "done. Found %d files.@\nLoading cache... @?" (Array.length files);
      files
      |> Array.to_list
      |> List.map load_from_cache
      |> List.iter (fun (report : Report.Package.t) ->
          Hashtbl.replace cache (Model.Package.path report.Report.Package.package) report);
      Format.eprintf "done. Loaded %d packages.@." (Hashtbl.length cache)
    )

(* ============================== [ Packages ] ============================== *)

let () = Format.eprintf "Reading packages list... @?"
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
let nb_packages = List.length package_pathes
let () = Format.eprintf "done. Found %d packages.@." nb_packages

let package_pathes =
  if config.cache <> "" then
    (
      Format.eprintf "Filtering packages found in cache... @?";
      let package_pathes = List.filter (fnot (Hashtbl.mem cache)) package_pathes in
      Format.eprintf "done. %d packages remain.@." (List.length package_pathes);
      package_pathes
    )
  else
    package_pathes

(* ============================== [ Contents ] ============================== *)

let () =
  if config.contents <> "" && package_pathes <> [] then
    (
      Format.eprintf "Reading contents files... @?";
      Contents.scan config.contents;
      Format.eprintf "done.@."
    )

(* ============================== [ Process ] =============================== *)

let pad_int_left_to y x =
  let len x = iof (ceil (log10 (foi x +. 1.))) in
  spf "%s%d" (String.make (max 0 (len y - len x)) ' ') x

let pad_string_right_to y x =
  spf "%s%s" x (String.make (max 0 (String.length y - String.length x)) ' ')

let previous = ref ""

let process_package i package_path =
  let () =
    let i = i + 1 in
    let str = Filename.basename package_path in
    Format.eprintf "\r  [%s/%d; %3d%%] .../%s@?" (pad_int_left_to nb_packages i) nb_packages (100 * i / nb_packages) (pad_string_right_to !previous str);
    previous := str
  in
  let package_name =
    let name_version = Filename.basename package_path in
    let l = String.index name_version '_' in
    String.sub name_version 0 l
  in
  let package = parse_package_from_dir ~content:(Contents.get_files package_name) package_path in
  let report = analyse_package ~config package in
  if config.cache <> "" then save_to_cache report;
  report

let (meta, reports) =
  Report.Meta.while_gathering_meta @@ fun () ->
  if package_pathes <> [] then
    (
      Format.eprintf "Processing packages...@.";
      let reports =
        MultiProcess.mapi_p ~workers:config.workers process_package package_pathes
        |> Lwt_main.run
      in
      Format.eprintf "@\ndone.@.";
      reports
    )
  else
    []

let reports = (* Append cached reports to computed ones *)
  Hashtbl.to_seq_values cache
  |> List.of_seq
  |> List.append reports

(* =============================== [ Report ] =============================== *)

let () = previous := ""

let generate_one_html_package_report i (report : Report.Package.t) =
  let () =
    let i = i + 1 in
    let str = Model.Package.name report.Report.Package.package in
    Format.eprintf "\r  [%s/%d; %3d%%] %s   @?" (pad_int_left_to nb_packages i) nb_packages (100 * i / nb_packages)
      (pad_string_right_to !previous str);
    previous := str
  in
  generate_html_package_report ~standalone:false ~prefix:config.report report

let () = Format.eprintf "Generating report...@."
let () =
  match reports with
  | [] -> failwith "no input"
  | [report] ->
    generate_html_package_report ~standalone:true ~prefix:config.report report
  | reports ->
    MultiProcess.iteri_p ~workers:config.workers generate_one_html_package_report reports
    |> Lwt_main.run;
    let report =
      reports
      |> List.map summarize_package_report
      |> make_batch_report ~meta ~config
      |> enrich_batch_report
    in
    generate_html_batch_report ~prefix:config.report report
let () = Format.eprintf "@\ndone.@."
