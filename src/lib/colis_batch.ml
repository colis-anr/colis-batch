module Model = Colis_batch_model
module Report = Colis_batch_report
module Engine = Colis_batch_engine

let parse_package_from_dir = Model.Package.parse

let analyse_package = Engine.Package.analyse

let save_package_report = Report.Package.save
let load_package_report = Report.Package.load

let summarize_package_report = Report.Package.summarize

let save_package_report_summary = Report.Package.save_summary
let load_package_report_summary = Report.Package.load_summary

let combine_reports = Report.Batch.make

let save_batch_report = Report.Batch.save
let load_batch_report = Report.Batch.load
