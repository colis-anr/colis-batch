open Colis_batch

let () =
  let package = parse_package_from_dir ~content:[] Sys.argv.(1) in
  assert (not (Sys.file_exists "report"));
  let report = analyse_package ~config:Config.default package in
  generate_html_package_report ~standalone:true ~prefix:"report" report
