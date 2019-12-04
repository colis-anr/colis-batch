open Colis_batch_ext
module Model = Colis_batch_model

type t =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    packages : Package.summary list }
[@@deriving yojson { exn = true }]

let make ~meta ~config packages =
  { meta ; config ; packages }

let save_as_json ~prefix report =
  report
  |> to_yojson
  |> Yojson.Safe.to_file (Path.main_json_file ~prefix)

let load_as_json ~prefix =
  Yojson.Safe.from_file (Path.main_json_file ~prefix)
  |> of_yojson_exn

(* ================================ [ Rich ] ================================ *)

type numbers =
  { packages : int ;
    scenarios : numbers_scenarios ;
    scripts : numbers_scripts }

and numbers_scenarios =
  { stotal : int ;
    per_package : int ;
    complete : int ;
    partial : int ;
    failure : int ;
    incomplete : int ;
    timeout : int ;
    out_of_memory : int ;
    not_converted : int ;
    unsupported_utility : int ;
    unexpected : int ;
    problems : int }

and numbers_scripts =
  { total : int ;
    parsing_errored : int ;
    parsing_rejected : int ;
    conversion_errored : int ;
    conversion_rejected : int ;
    accepted : int }
[@@deriving yojson]

type utility =
  { name : string ;
    options : (string list * int) list ;
    occurrences : int ;
    score : float }
(* FIXME: instead of int, list of package + script + position *)
[@@deriving yojson]

type scripts =
  { utilities : utility list } (* FIXME: move number here *)
[@@deriving yojson]

type rich =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    numbers : numbers ;
    scripts : scripts ;
    packages : Package.summary list }
[@@deriving yojson { exn = true }]

let enrich_numbers (report : t) : numbers =
  let nb_packages = List.length report.packages in
  let scenarios =
    let per_package = List.length Model.Scenarii.all in
    let stotal = nb_packages * per_package in
    let complete = ref 0 in
    let partial = ref 0 in
    let incomplete = ref 0 in
    let timeout = ref 0 in
    let out_of_memory = ref 0 in
    let not_converted = ref 0 in
    let unsupported_utility = ref 0 in
    let unexpected = ref 0 in
    List.iter
      (fun package ->
         List.iter
           (fun (_name, scenario) ->
              let open Model.Scenario in
              match coverage scenario with
              | Null r ->
                if ran_node_gen_incomplete r then incr incomplete;
                if ran_node_gen_timeout r then incr timeout;
                if ran_node_gen_oomemory r then incr out_of_memory;
                if ran_node_gen_notconverted r then incr not_converted;
                if ran_node_gen_has_unsupported r then incr unsupported_utility;
                if ran_node_gen_has_unexpected r then incr unexpected
              | Partial r ->
                incr partial;
                if ran_node_gen_incomplete r then incr incomplete;
                if ran_node_gen_timeout r then incr timeout;
                if ran_node_gen_oomemory r then incr out_of_memory;
                if ran_node_gen_notconverted r then incr not_converted;
                if ran_node_gen_has_unsupported r then incr unsupported_utility;
                if ran_node_gen_has_unexpected r then incr unexpected
              | Complete -> incr complete)
           package.Package.scenarii)
      report.packages;
    let problems =
      !incomplete + !timeout + !out_of_memory
      + !not_converted + !unsupported_utility + !unexpected
    in
    { stotal ; per_package ;
      complete = !complete ;
      partial = !partial ;
      failure = stotal - !complete - !partial ;
      incomplete = !incomplete ;
      timeout = !timeout ;
      out_of_memory = !out_of_memory ;
      not_converted = !not_converted ;
      unsupported_utility = !unsupported_utility ;
      unexpected = !unexpected ;
      problems }
  in
  let scripts =
    let parsing_errored = ref 0 in
    let parsing_rejected = ref 0 in
    let conversion_errored = ref 0 in
    let conversion_rejected = ref 0 in
    let accepted = ref 0 in
    let scripts =
      List.flat_map
        (fun package ->
           let package = package.Package.package in
           List.map
             (fun script -> (package, script))
             (Model.Package.maintscripts package))
        report.packages
    in
    List.iter
      (fun package_script ->
         let (_, script) = package_script in
         incr
           (match Model.Maintscript.error script with
            | None -> accepted
            | Some (ParsingErrored _) -> parsing_errored
            | Some (ParsingRejected _) -> parsing_rejected
            | Some (ConversionErrored _) -> conversion_errored
            | Some (ConversionRejected _) -> conversion_rejected))
      scripts;
    { total = List.length scripts ;
      parsing_errored = !parsing_errored ;
      parsing_rejected = !parsing_rejected ;
      conversion_errored = !conversion_errored ;
      conversion_rejected = !conversion_rejected ;
      accepted = !accepted }
  in
  { packages = nb_packages ; scenarios ; scripts }

let enrich_scripts (report : t) : scripts =
  let utilities_table = Hashtbl.create 8 in
  List.iter
    (fun package ->
       Model.Package.iter_maintscripts
         (fun script ->
            if not (Model.Maintscript.has_error script) then
              (
                let utilities = Model.Maintscript.utilities script in
                let nb_unsupported =
                  utilities
                  |> List.map fst
                  |> List.filter (fnot Colis.Symbolic.Utility.is_registered)
                  |> List.length
                  |> foi
                in
                List.iter
                  (fun (name, args) ->
                     let utility =
                       match Hashtbl.find_opt utilities_table name with
                       | None -> { name ; options = [] ; occurrences = 0 ; score = 0. }
                       | Some utility -> utility
                     in
                     let options =
                       List.fold_left
                         (fun options args ->
                            List.update_assoc
                              args
                              (function
                                | None -> Some 1
                                | Some nb -> Some (nb + 1))
                              options)
                         utility.options
                         args
                     in
                     let occurrences =
                       utility.occurrences
                       + List.length args
                     in
                     let score =
                       if Colis.Symbolic.Utility.is_registered name then
                         0.
                       else
                         utility.score +. 1. /. nb_unsupported
                     in
                     Hashtbl.replace utilities_table name
                       { name ; options ; occurrences ; score }
                  )
                  utilities
              )
         )
         package.Package.package)
    report.packages;
  let utilities =
    utilities_table
    |> Hashtbl.to_seq
    |> Seq.map snd
    |> List.of_seq
  in
  { utilities }

let enrich (report : t) : rich =
  { meta = report.meta ;
    config = report.config ;
    numbers = enrich_numbers report ;
    scripts = enrich_scripts report ;
    packages = report.packages }
