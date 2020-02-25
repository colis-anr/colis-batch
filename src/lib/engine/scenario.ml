open Colis_batch_ext
module Model = Colis_batch_model
open Model.Scenario

let fhs =
  "bin
boot
dev
etc
etc/opt
etc/X11
etc/sgml
etc/xml
home
lib
mnt
media
opt
proc
root
run
run/lock
sbin
srv
tmp
usr
usr/bin
usr/include
usr/lib
usr/sbin
usr/share
usr/share/man
usr/share/misc
usr/src
usr/X11R6
usr/local
usr/local/bin
usr/local/etc
usr/local/games
usr/local/include
usr/local/lib
usr/local/man
usr/local/sbin
usr/local/share
usr/local/src
var
var/cache
var/lib
var/lib/misc
var/lock
var/log
var/opt
var/run
var/spool
var/spool/cron
var/spool/mail
var/spool/mqueue
var/tmp
var/www"
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char '/')
  |> List.fold_left
    (fun fhs dir -> Colis.Symbolic.FilesystemSpec.add_dir dir fhs)
    Colis.Symbolic.FilesystemSpec.empty

exception NotConverted

let run_script ~cmd_line_arguments ~states ~package ~script =
  match Model.Package.maintscript package script with
  | None -> assert false (*(states, [], [])*)
  | Some script ->
    try
      Model.Maintscript.interp (* FIXME: that has to move to Engine *)
        ~cmd_line_arguments
        ~states
        ~package_name:(Model.Package.name package)
        script
    with
      Invalid_argument _ -> raise NotConverted

let run ~cpu_timeout ~package scenario =
  let rec run states (scenario : (unit, unit) t) : (ran_leaf, ran_node) t =
    match scenario with
    | Status ((), status) -> Status (states, status)

    | Unpack ((), on_success) ->
      (
        let content = Model.Package.content package in
        if content = [] then
          Unpack (make_ran_node ~absent:true states, run states on_success)
        else
          (
            let unpack_script =
              let open Colis.Language.SyntaxHelpers in
              content
              |> List.map (fun file -> icallutility "colis_internal_unsafe_touch" [lliteral file])
              |> List.cons (icallutility "true" []) (* in case of empty list *)
              |> isequence_l |> program
            in
            let sym_states = List.map (Colis.Symbolic.to_symbolic_state ~vars:[] ~arguments:[]) states in
            let (success, _error, incomplete) =
              Colis.Constraints.Clause.with_shadow_variables @@ fun () ->
              Colis.Symbolic.interp_program
                ~loop_limit:200
                ~stack_size:200
                ~argument0:"colis_internal_unpack"
                sym_states unpack_script
            in
            assert (incomplete = []);
            Unpack (make_ran_node states, run success on_success)
          )
      )

    | RunScript ((), (script, cmd_line_arguments), on_success, on_error) ->
      (
        if not (Model.Package.has_maintscript package script) then
          RunScript (make_ran_node ~absent:true states, (script, cmd_line_arguments),
                     run states on_success, run [] on_error)
        else if states = [] then
          RunScript (make_ran_node [], (script, cmd_line_arguments),
                     run [] on_success, run [] on_error)
        else
          let (success, error, ran_node) =
            try
              let (success, error, incomplete) =
                run_script ~cmd_line_arguments ~states ~package ~script
              in
              (success, error, make_ran_node ~incomplete:(incomplete<>[]) states)
            with
            | Colis.Internals.Errors.Unsupported (utility, msg) ->
              ([], [], make_ran_node ~unsupported:[utility, msg] states)
            | Colis.Internals.Errors.CpuTimeLimitExceeded ->
              ([], [], make_ran_node ~timeout:true states)
            | Colis.Internals.Errors.MemoryLimitExceeded ->
              ([], [], make_ran_node ~oomemory:true states)
            | NotConverted ->
              ([], [], make_ran_node ~notconverted:true states)
            | exn ->
              ([], [], make_ran_node ~unexpected:[exn] states)
          in
          RunScript (ran_node, (script, cmd_line_arguments),
                     run success on_success, run error on_error)
      )
  in
  try
    Colis.Internals.Options.cpu_time_limit := Sys.time () +. cpu_timeout;
    let root = Colis.Constraints.Var.fresh ~hint:"r" () in
    let disj =
      Colis.Constraints.Clause.with_shadow_variables @@ fun () ->
      Colis.Symbolic.add_fs_spec_to_clause root Colis.Constraints.Clause.true_sat_conj fhs
    in
    let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
    run stas scenario
  with
  (* FIXME: maybe a cleaner way would be to have an initial node of a scenario? *)
  | Colis.Internals.Errors.CpuTimeLimitExceeded ->
    (
      match scenario with
      | RunScript ((), (script, cmd_line_arguments), on_success, on_error) ->
        RunScript (make_ran_node ~timeout:true [], (script, cmd_line_arguments),
                   run [] on_success, run [] on_error)
      | _ -> assert false
    )
  | Colis.Internals.Errors.MemoryLimitExceeded ->
    (
      match scenario with
      | RunScript ((), (script, cmd_line_arguments), on_success, on_error) ->
        RunScript (make_ran_node ~oomemory:true [], (script, cmd_line_arguments),
                   run [] on_success, run [] on_error)
      | _ -> assert false
    )
