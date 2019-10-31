module Model = Colis_batch_model
module Report = Colis_batch_report
module Engine = Colis_batch_engine

let parse_package_from_dir = Model.Package.parse

let analyse_package = Engine.Package.analyse
