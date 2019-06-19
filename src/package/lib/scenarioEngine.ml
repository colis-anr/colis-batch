open Colis_ext
open Scenario

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
sbin
srv
tmp
usr
usr/bin
usr/include
usr/lib
usr/sbin
usr/share
usr/src
usr/X11R6
usr/local
var
var/cache
var/lock
var/log
var/mail
var/run
var/spool
var/spool/cron
var/spool/mail
var/spool/mqueue
var/tmp"
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char '/')
  |> List.fold_left
    (fun fhs dir -> Colis.Symbolic.FilesystemSpec.add_dir dir fhs)
    Colis.Symbolic.FilesystemSpec.empty

let run_script ~cpu_timeout ~cmd_line_arguments ~states ~package ~script =
  match Package.maintscript package script with
  | None -> (states, [], [])
  | Some script ->
    Maintscript.interp
      ~cpu_timeout
      ~cmd_line_arguments
      ~states
      ~package_name:(Package.name package)
      script

let run ~cpu_timeout ~package scenario =
  let rec run states (scenario : (unit, unit) t) : (ran_leaf, ran_node) t =
    match scenario with
    | Status ((), status) -> Status (states, status)
    | Unpack ((), on_success) ->
      Unpack (make_ran_node (), run states on_success) (* FIXME!!! *)
    | RunScript ((), (script, cmd_line_arguments), on_success, on_error) ->
      let (success, error, ran_node) =
        try
          let (success, error, incomplete) =
            run_script ~cpu_timeout ~cmd_line_arguments ~states ~package ~script
          in
          (success, error, make_ran_node ~incomplete:(incomplete<>[]) ())
        with
        | Colis.Errors.Unsupported (utility, msg) ->
          ([], [], make_ran_node ~unsupported:(utility, msg) ())
        | Constraints_common.Log.CPU_time_limit_exceeded ->
          ([], [], make_ran_node ~timeout:true ())
        | exn ->
          ([], [], make_ran_node ~unexpected:exn ())
      in
      RunScript (ran_node, (script, cmd_line_arguments), run success on_success, run error on_error)
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fhs in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  run stas scenario
