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

let run_script ~cmd_line_arguments ~states ~package script =
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

let run_script_and_sort_status ~cmd_line_arguments ~states ~package ~success_outcome ~error_outcome ~incomplete_outcome script =
  try
    let (success, error, incomplete) =
      run_script ~cmd_line_arguments ~states ~package script
    in
    List.map (fun success -> (success, success_outcome)) success
    @ List.map (fun error -> (error, error_outcome)) error
    @ List.map (fun incomplete -> (incomplete, incomplete_outcome)) incomplete
  with
    Constraints_common.Log.CPU_time_limit_exceeded ->
    List.map (fun sta -> (sta, Error Timeout)) states

let install package =
  let postinst_configure states =
    run_script_and_sort_status
      ~cmd_line_arguments:["configure"]
      ~states ~package
      ~success_outcome:(Ok Installed)
      ~error_outcome:(Ok FailedConfig)
      ~incomplete_outcome:(Error Incomplete)
      Maintscript.Postinst
  in
  let files_unpacking states =
    postinst_configure states (* FIXME *)
  in
  let postrm_abort_install states =
    run_script_and_sort_status
      ~cmd_line_arguments:["abort-install"]
      ~states ~package
      ~success_outcome:(Ok NotInstalled)
      ~error_outcome:(Ok HalfInstalled)
      ~incomplete_outcome:(Error Incomplete)
      Maintscript.Postrm
  in
  let preinst_install states =
    let (success, error, incomplete) =
      run_script
        ~cmd_line_arguments:["install"]
        ~states ~package
        Maintscript.Preinst
    in
    files_unpacking success
    @ postrm_abort_install error
    @ List.map (fun incomplete -> (incomplete, Error Incomplete)) incomplete
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let fs_spec = Colis.Symbolic.FilesystemSpec.empty in
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fs_spec in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  preinst_install stas
