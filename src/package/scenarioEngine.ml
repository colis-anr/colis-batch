open Colis_ext
open Scenario

let run_script ~cmd_line_arguments ~states ~package ~script =
  Maintscript.interp
    ~cmd_line_arguments
    ~states
    ~key:script
    (Package.maintscript package script)

let create_report ~package ~name ran =
  let categorize ran =
    let rec categorize ran =
      match ran.scenario with
      | Status status ->
        ran.data.states
        |> List.map (fun sta -> (status, sta))
      | Action (_, on_success, on_error) ->
        categorize on_success
        @ categorize on_error
    in
    categorize ran
    |> List.sort compare
    |> List.group compare
  in
  let ran = categorize ran in
  let package = Package.name package in
  let scenario = name_to_string name in
  let path = ["package"; package; "scenario"; scenario; "index.html"] in
  (HtmlReport.with_formatter_to_report ~viz:true path @@ fun fmt ->
   HtmlReport.Scenario.pp_package fmt ~package scenario ran);
  List.iter
    (fun (status, states) ->
       List.iteri
         (fun id state ->
            let path = ["package"; package; "scenario"; scenario; Scenario.Status.to_string status; (string_of_int id) ^ ".dot"] in
            (HtmlReport.with_formatter_to_file path @@ fun fmt ->
             let clause = state.Colis.Symbolic.Semantics.filesystem.clause in
             Colis.Constraints.Clause.pp_sat_conj_as_dot ~name:(Format.asprintf "%a-%d" Scenario.Status.pp status id) fmt clause);
            let path = ["package"; package; "scenario"; scenario; Scenario.Status.to_string status; (string_of_int id) ^ ".html"] in
            (HtmlReport.with_formatter_to_report ~viz:true path @@ fun fmt ->
             HtmlReport.Scenario.pp_state fmt ~package ~status ~id state)
         )
         states)
    ran

let create_flowchart ~package ~name ran =
  let path = ["package"; Package.name package; "scenario"; name_to_string name; "flowchart.dot"] in
  HtmlReport.with_formatter_to_file path @@ fun fmt ->
  pp_ran_as_dot ~name fmt ran

let run ~package ~name scenario =
  let rec run states (scenario : unit t) : ran t =
    match scenario.scenario with
    | Status status ->
      { scenario = Status status ;
        data = { states ; incomplete = [] ; timeout = false } }
    | Action (action, on_success, on_error) ->
      match action with
      | RunScript (script, cmd_line_arguments) ->
        let ((success, error, incomplete), timeout) =
          try
            (run_script ~cmd_line_arguments ~states ~package ~script, false)
          with
            Constraints_common.Log.CPU_time_limit_exceeded ->
            (([], [], []), true)
        in
        { scenario = Action (action, run success on_success, run error on_error) ;
          data = { states ; incomplete ; timeout } }
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let fs_spec = Colis.Symbolic.FilesystemSpec.empty in (* FIXME *)
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fs_spec in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  let ran = run stas scenario in
  create_report ~package ~name ran;
  create_flowchart ~package ~name ran;
  ran
