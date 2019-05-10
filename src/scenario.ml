type success_outcome =
  | Installed
  | FailedConfig
  | NotInstalled
  | HalfInstalled
  | ConfigFiles
  | Unpacked

type error_outcome =
  | Incomplete
  | Timeout

type outcome = (success_outcome, error_outcome) result

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

let set_outcome o = List.map (fun x -> (x, o))

let run_script
    ~package ~script
    ~cmd_line_arguments
    ~states
    ~on_success_states ~on_error_states ~on_incomplete_states
  =
  try
    let (success, error, incomplete) =
      run_script ~cmd_line_arguments ~states ~package ~script
    in
    on_success_states success
    @ on_error_states error
    @ on_incomplete_states incomplete
  with
    Constraints_common.Log.CPU_time_limit_exceeded ->
    [(Obj.magic (), Error Timeout)] (* FIXME *)

let install package =
  let postinst_configure states =
    run_script
      ~package ~script:Maintscript.Postinst
      ~cmd_line_arguments:["configure"]
      ~states
      ~on_success_states:(set_outcome (Ok Installed))
      ~on_error_states:(set_outcome (Ok FailedConfig))
      ~on_incomplete_states:(set_outcome (Error Incomplete))
  in
  let files_unpacking states =
    postinst_configure states (* FIXME *)
  in
  let postrm_abort_install states =
    run_script
      ~package ~script:Maintscript.Postrm
      ~cmd_line_arguments:["abort-install"]
      ~states
      ~on_success_states:(set_outcome (Ok NotInstalled))
      ~on_error_states:(set_outcome (Ok HalfInstalled))
      ~on_incomplete_states:(set_outcome (Error Incomplete))
  in
  let preinst_install states =
    run_script
      ~package ~script:Maintscript.Preinst
      ~cmd_line_arguments:["install"]
      ~states
      ~on_success_states:files_unpacking
      ~on_error_states:postrm_abort_install
      ~on_incomplete_states:(set_outcome (Error Incomplete))
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let fs_spec = Colis.Symbolic.FilesystemSpec.empty in (* FIXME *)
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fs_spec in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  preinst_install stas
