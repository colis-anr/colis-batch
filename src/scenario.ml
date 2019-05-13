let fpf = Format.fprintf
let spf = Format.sprintf

let run_script ~cmd_line_arguments ~states ~package ~script =
  match Package.maintscript package script with
  | None -> (states, [], []) (* Trivial success *)
  | Some shell ->
    (* Assertion: it works because we have tried before. *)
    let colis = Colis.convert_shell_file ~cmd_line_arguments shell in
    let sym_states =
      List.map
        (Colis.Symbolic.to_symbolic_state
           ~vars:[] (* FIXME *)
           ~arguments:cmd_line_arguments)
        states
    in
    Constraints_common.Log.cpu_time_limit := Some (Sys.time () +. !Options.cpu_timeout);
    Colis.Symbolic.interp_program
      ~loop_limit:200
      ~stack_size:200
      ~argument0:(Maintscript.name_to_string script)
      sym_states colis

type status =
  | Installed
  | FailedConfig
  | NotInstalled
  | HalfInstalled
  | ConfigFiles
  | Unpacked

let pp_status fmt status =
  fpf fmt "%s" (match status with
      | Installed -> "Installed"
      | FailedConfig -> "Failed-Config"
      | NotInstalled -> "Not-Installed"
      | HalfInstalled -> "Half-Installed"
      | ConfigFiles -> "Config-Files"
      | Unpacked -> "Unpacked")

type action =
  (* FIXME: unpack *)
  | RunScript of Maintscript.name * string list

let pp_action fmt = function
  | RunScript (script, args) ->
    fpf fmt "%s" (Maintscript.name_to_string script);
    List.iter (fpf fmt " %s") args

type 'a scenario =
  | Status of status
  | Action of action * 'a t * 'a t

and 'a t =
  { data : 'a ;
    scenario : 'a scenario }

let action ~action ~on_success ~on_error =
  { data = () ; scenario = Action (action, on_success, on_error) }

let status status =
  { data = () ; scenario = Status status }

let rec pp pp_a fmt sc =
  let pp_title fmt = function
    | Status status -> fpf fmt "Status: %a" pp_status status
    | Action (action, _, _) -> pp_action fmt action
  in
  pp_title fmt sc.scenario;
  fpf fmt "@\n@[%a@]" pp_a sc.data;
  match sc.scenario with
  | Status _ -> ()
  | Action (_, on_success, on_error) ->
    fpf fmt "@\n@\nS-- @[%a@]@\n@\nE-- @[%a@]"
      (pp pp_a) on_success
      (pp pp_a) on_error

type colis_state = Colis.Symbolic.Semantics.state

type ran =
  { states : colis_state list ;
    incomplete : colis_state list ;
    timeout : bool }

type name =
  | Install

let name_to_string = function
  | Install -> "install"

let pp_ran fmt ran =
  fpf fmt "(states before: %d%s%s)"
    (List.length ran.states)
    (let s = List.length ran.incomplete in
     if s = 0 then "" else spf "; %d incomplete" s)
    (if ran.timeout then "; timeout" else "")

let pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc =
  let rec pp_as_dot ?parent fmt sc =
    let n = Hashtbl.hash sc in
    (
      match sc.scenario with
      | Status status ->
        fpf fmt "%d [label=\"%a\",shape=none];@\n"
          n
          pp_status status
      | Action (action, on_success, on_error) ->
        fpf fmt "%d [label=\"{%a%a}\"];@\n@\n@[<h 2>  %a@]@\n@[<h 2>  %a@]@\n"
          n
          pp_action action
          pp_action_label sc
          (pp_as_dot ~parent:("OK", n)) on_success
          (pp_as_dot ~parent:("Failed", n)) on_error
    );
    (
      match parent with
      | None -> ()
      | Some (kind, parent) ->
        fpf fmt "%d -> %d [label=\"%s%a\"];@\n"
          parent n kind
          pp_edge_label sc
    )
  in
  fpf fmt "digraph %s {@\n@[<h 2>  node[shape=Mrecord];@\n@\n%a@]}@."
    (name_to_string name) (pp_as_dot ?parent:None) sc

let pp_unit_as_dot ~name fmt sc =
  let pp_nop _ _ = () in
  pp_as_dot ~pp_action_label:pp_nop ~pp_edge_label:pp_nop ~name fmt sc

let pp_ran_as_dot ~name fmt sc =
  let pp_action_label fmt sc =
    (match List.length sc.data.incomplete with
     | 0 -> ()
     | l -> fpf fmt "|%d incomplete" l);
    (if sc.data.timeout then
       fpf fmt "|timeout")
  in
  let pp_edge_label fmt sc =
    match List.length sc.data.states with
    | 0 -> ()
    | l -> fpf fmt "\\n%d" l
  in
  pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc

let install = (* FIXME: unpack *)
  action
    ~action:(RunScript (Maintscript.Preinst, ["install"]))
    ~on_success:(
      action
        ~action:(RunScript (Maintscript.Postinst, ["configure"]))
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )
    ~on_error:(
      action
        ~action:(RunScript (Maintscript.Postrm, ["abort-install"]))
        ~on_success:(status NotInstalled)
        ~on_error:(status HalfInstalled)
    )

let all =
  [ Install, install ]

type output =
  | Status of status * Colis.Symbolic.Semantics.state list (* FIXME *)
  | Action of output * output
  | ActionTimeout

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
  ReportHelpers.(with_formatter_to_file
                   (scenario_path
                      ~package:(Package.name package)
                      ~scenario:(name_to_string name)
                      "flowchart.dot"))
  @@ fun fmt ->
  pp_ran_as_dot ~name fmt ran;
  ran

let run ~package name =
  run ~package ~name (List.assoc name all)
