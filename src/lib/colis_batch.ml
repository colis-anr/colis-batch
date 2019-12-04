module Config = Colis_batch_config
module Model = Colis_batch_model
module Report = Colis_batch_report
module Engine = Colis_batch_engine

let parse_package_from_dir = Model.Package.parse

let analyse_package = Engine.Package.analyse

let save_package_report_as_bin = Report.Package.save_as_bin
let load_package_report_as_bin = Report.Package.load_as_bin

let generate_html_package_report = Report.Html.Package.generate

let summarize_package_report = Report.Package.summarize

let make_batch_report = Report.Batch.make

let enrich_batch_report = Report.Batch.enrich

let generate_html_batch_report = Report.Html.Batch.generate
