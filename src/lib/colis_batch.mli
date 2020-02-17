(** {1 CoLiS-Batch} *)

(** {2 Acces to Deeper Modules}

    CoLiS-Batch is made of three main parts. The main functions of these parts
    are made available directly in this main module. But everything else is
    accessible from these submodules. *)

module Config = Colis_batch_config
(** {!Config} contains the configuration options. *)

module Model = Colis_batch_model
(** {!Model} contains the internal definitions [CoLiS-Batch] uses: scripts,
    packages, scenarios, etc. *)

module Report = Colis_batch_report
(** {!Report} contains the definition of what [CoLiS-Batch] reports, as well as
    functions to save reports, generate CLI and HTML reports, etc. *)

module Engine = Colis_batch_engine
(** {!Engine} contains the functions that run scripts in the right environment,
    run scenarios on packages, etc. *)

(** {2 One Package} *)

val parse_package_from_dir : string -> Model.Package.t

val analyse_package : config:Config.t -> Model.Package.t -> Report.Package.t

val load_package_report_as_bin : cache:string -> string -> Report.Package.t
val save_package_report_as_bin : cache:string -> string -> Report.Package.t -> unit

val generate_html_package_report : standalone:bool -> prefix:string -> Report.Package.t -> unit

val summarize_package_report : Report.Package.t -> Report.Package.summary

(** {2 A Batch of Packages} *)

val make_batch_report :
  meta:Report.Meta.t -> config:Config.t ->
  Report.Package.summary list ->
  Report.Batch.t

val enrich_batch_report : Report.Batch.t -> Report.Batch.rich

val generate_html_batch_report : prefix:string -> Report.Batch.rich -> unit
