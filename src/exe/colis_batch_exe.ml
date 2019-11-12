open Colis_batch

let prefix = "report"
let config = Config.default

let () =
  assert (not (Sys.file_exists prefix));
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
    generate_html_package_report ~standalone:true ~prefix report
  | reports ->
    List.iter (generate_html_package_report ~standalone:false ~prefix) reports;
    let report =
      reports
      |> List.map summarize_package_report
      |> make_batch_report ~meta ~config
    in
    generate_html_batch_report ~prefix report
