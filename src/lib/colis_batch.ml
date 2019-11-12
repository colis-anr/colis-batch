module Config = Colis_batch_config
module Model = Colis_batch_model
module Report = Colis_batch_report
module Engine = Colis_batch_engine

let parse_package_from_dir = Model.Package.parse

let analyse_package = Engine.Package.analyse

let save_package_report_as_json = Report.Package.save_as_json
let load_package_report_as_json = Report.Package.load_as_json

let summarize_package_report = Report.Package.summarize

let save_package_report_summary_as_json = Report.Package.save_summary_as_json
let load_package_report_summary_as_json = Report.Package.load_summary_as_json

let save_package_report_summary_as_bin = Report.Package.save_summary_as_bin
let load_package_report_summary_as_bin = Report.Package.load_summary_as_bin

let make_batch_report = Report.Batch.make

let save_batch_report_as_json = Report.Batch.save_as_json
let load_batch_report_as_json = Report.Batch.load_as_json
