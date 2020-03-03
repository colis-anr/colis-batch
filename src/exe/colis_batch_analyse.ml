open Colis_batch open Colis_batch_ext

let config = !Config.config

let () =
  Colis.Internals.Options.cpu_time_limit := config.cpu_timeout;
  Colis.Internals.Options.set_memory_limit config.memory_limit;
  Colis.Internals.Options.external_sources := config.external_sources;
  Colis.Internals.Options.unknown_behaviour := Exception

let pad_int_left_to y x =
  let len x = iof (ceil (log10 (foi x +. 1.))) in
  spf "%s%d" (String.make (max 0 (len y - len x)) ' ') x

let pad_string_right_to y x =
  spf "%s%s" x (String.make (max 0 (String.length y - String.length x)) ' ')

let (package_pathes, nb_packages) =
  epf "Reading packages list... @?";
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
  in
  let nb_packages = List.length package_pathes in
  epf "done. Found %d packages.@." nb_packages;
  (package_pathes, nb_packages)

let () =
  if nb_packages = 0 then
    (
      epf "No package to handle. Exiting.@.";
      exit 0
    )

(* FIXME: add a way to recover from non-empty cache. *)
let process_package i package_path =
  let () =
    let i = i + 1 in
    let str = Filename.basename package_path in
    Format.eprintf "\r  [%s/%d; %3d%%] .../%s@?" (pad_int_left_to nb_packages i) nb_packages (100 * i / nb_packages) str;
  in

  let package = parse_package_from_dir package_path in
  let report = analyse_package ~config package in
  save_package_report_as_bin ~cache:config.cache (spf "%x.bin" (Hashtbl.hash package_path)) report

let (meta, reports) =
  Report.Meta.while_gathering_meta @@ fun () ->
  epf "Processing packages...@.";
  let () =
    MultiProcess.iteri_p ~workers:config.workers process_package package_pathes
    |> Lwt_main.run
  in
  epf "@\ndone.@."

let () =
  epf "Writing meta to cache... @?";
  Filesystem.write_value_to_file meta (Filename.concat config.cache "meta.bin");
  epf "done.@."
