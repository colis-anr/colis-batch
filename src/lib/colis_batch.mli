module Model = Colis_batch_model
module Report = Colis_batch_report

val parse_package_from_dir : content:string list -> string -> Model.Package.t

val analyse_package : Model.Package.t -> Report.Package.t
