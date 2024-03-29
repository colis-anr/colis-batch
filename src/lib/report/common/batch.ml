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
    unknown_utility : int ;
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
    options : (
      (string list * int) (* dashed + number of non-dashed options *)
      * (Model.Package.t * Model.Maintscript.t) list (* FIXME: position *)
    ) list ;
    occurrences : (Model.Package.t * Model.Maintscript.t) list (* FIXME: position *) ;
    score : float }
[@@deriving yojson]

type scripts =
  { utilities : utility list } (* FIXME: move number here *)
[@@deriving yojson]

type scenario =
  { scenario : Model.Scenario.clean ;
    packages_by_status : (Model.Scenario.Status.t * Package.summary list) list }
[@@deriving yojson]

type rich =
  { meta : Meta.t ;
    config : Colis_batch_config.t ;
    numbers : numbers ;
    scripts : scripts ;
    packages : Package.summary list ;
    scenarios : (Model.Scenarii.Name.t * scenario) list }
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
    let unknown_utility = ref 0 in
    let unexpected = ref 0 in
    (
      report.packages |> List.iter @@ fun package ->
      package.Package.scenarii |> List.iter @@ fun (_name, scenario) ->
      let open Model.Scenario in
      match coverage scenario with
      | Null r ->
        if ran_node_gen_incomplete r then incr incomplete;
        if ran_node_gen_timeout r then incr timeout;
        if ran_node_gen_oomemory r then incr out_of_memory;
        if ran_node_gen_notconverted r then incr not_converted;
        if ran_node_gen_has_unknown r then incr unknown_utility;
        if ran_node_gen_has_unexpected r then incr unexpected
      | Partial r ->
        incr partial;
        if ran_node_gen_incomplete r then incr incomplete;
        if ran_node_gen_timeout r then incr timeout;
        if ran_node_gen_oomemory r then incr out_of_memory;
        if ran_node_gen_notconverted r then incr not_converted;
        if ran_node_gen_has_unknown r then incr unknown_utility;
        if ran_node_gen_has_unexpected r then incr unexpected
      | Complete -> incr complete
    );
    let problems =
      !incomplete + !timeout + !out_of_memory
      + !not_converted + !unknown_utility + !unexpected
    in
    { stotal ; per_package ;
      complete = !complete ;
      partial = !partial ;
      failure = stotal - !complete - !partial ;
      incomplete = !incomplete ;
      timeout = !timeout ;
      out_of_memory = !out_of_memory ;
      not_converted = !not_converted ;
      unknown_utility = !unknown_utility ;
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
  (* For each package report summary *)
  report.packages |> List.iter (fun package ->
      (* For each maintscript in the package *)
      package.Package.package |> Model.Package.iter_maintscripts (fun script ->
          (* If it has been converted without error *)
          if not (Model.Maintscript.has_error script) then
            (
              (* We gather its utilities. This is a list of utility
                 names, with a list of abstractions of the ways they
                 are called (with a list of dashed arguments and the
                 number of non-dashed ones). *)
              let utilities = Model.Maintscript.utilities script in
              let nb_unknown =
                utilities
                |> List.map fst
                |> List.filter (fun name -> not (Colis.SymbolicConstraints.is_registered ~name))
                |> List.length
                |> foi
              in
              (* Iterate through all the utility calls in this
                 script. *)
              utilities |> List.iter (fun (name, args) ->
                  (* Recover what we know on this utility on all
                     scripts. If we have never seen the utility
                     before, add it. *)
                  let utility =
                    match Hashtbl.find_opt utilities_table name with
                    | None -> { name ; options = [] ; occurrences = [] ; score = 0. }
                    | Some utility -> utility
                  in
                  (* Add to already existing options the ones coming
                     from this script. *)
                  let options =
                    List.fold_left
                      (fun options args ->
                         List.update_assoc
                           args
                           (fun places ->
                              let places = match places with None -> [] | Some places -> places in
                              Some ((package.Package.package, script) :: places)) (* FIXME: position *)
                           options)
                      utility.options
                      args
                  in
                  (* Add to already gathered occurrences the ones
                     coming from this script. *)
                  let occurrences =
                    List.map (fun _ -> (package.Package.package, script)) args (* FIXME: position? *)
                    @ utility.occurrences
                  in
                  let score =
                    if Colis.SymbolicConstraints.is_registered ~name then
                      0.
                    else
                      utility.score +. 1. /. nb_unknown
                  in
                  Hashtbl.replace utilities_table name
                    { name ; options ; occurrences ; score }))));
  let utilities =
    utilities_table
    |> Hashtbl.to_seq
    |> Seq.map snd
    |> List.of_seq
  in
  { utilities }

let enrich_scenario (report : t) name scenario =
  let all_status = Model.Scenario.all_status scenario in
  let packages_by_status =
    let packages_by_status =
      List.map (fun status -> (status, ref [])) all_status
    in
    report.packages |> List.iter (fun package ->
        package.Package.scenarii |> List.iter (fun (name', scenario) ->
            if name' = name then
              (
                scenario |> Model.Scenario.states_sum |> List.iter (fun (status, states) ->
                    if states <> 0 then
                      match List.assoc_opt status packages_by_status with
                      | None -> ()
                      | Some others -> others := package :: !others)
              )
          )
      );
    packages_by_status |> List.map (fun (status, packages) ->
        (status,
         List.sort_uniq
           (fun p1 p2 ->
              compare
                (Model.Package.name p1.Package.package)
                (Model.Package.name p2.Package.package))
           !packages))
  in
  { scenario ; packages_by_status }

let enrich_scenarios (report : t) =
  Model.Scenarii.all |> List.map (fun (name, sc) ->
      (name, enrich_scenario report name sc)
    )

let enrich (report : t) : rich =
  { meta = report.meta ;
    config = report.config ;
    numbers = enrich_numbers report ;
    scripts = enrich_scripts report ;
    packages = report.packages ;
    scenarios = enrich_scenarios report }
