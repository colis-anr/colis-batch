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
  { states_before : colis_state list ;
    states_after : colis_state list ;
    incomplete : colis_state list ;
    timeout : bool }

let pp_ran fmt ran =
  fpf fmt "(states before: %d; after: %d%s%s)"
    (List.length ran.states_before)
    (List.length ran.states_after)
    (let s = List.length ran.incomplete in
     if s = 0 then "" else spf "; %d incomplete" s)
    (if ran.timeout then "; timeout" else "")

type name =
  | Installation

let name_to_string = function
  | Installation -> "Installation"

let installation = (* FIXME: unpack *)
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
  [ Installation, installation ]

type output =
  | Status of status * Colis.Symbolic.Semantics.state list (* FIXME *)
  | Action of output * output
  | ActionTimeout

let run ~package scenario =
  let rec run states (scenario : unit t) : ran t =
    match scenario.scenario with
    | Status status ->
      { scenario = Status status ;
        data = { states_before = states ; states_after = states ; incomplete = [] ; timeout = false } }
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
          data = { states_before = states ; states_after = success @ error @ incomplete ; incomplete ; timeout } } (* FIXME: include incompletes? *)
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let fs_spec = Colis.Symbolic.FilesystemSpec.empty in (* FIXME *)
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fs_spec in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  run stas scenario

let install package = run ~package installation
