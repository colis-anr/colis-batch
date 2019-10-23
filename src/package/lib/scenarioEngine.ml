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

let run_script ~cmd_line_arguments ~states ~package ~script =
  match Package.maintscript package script with
  | None -> (states, [], [])
  | Some script ->
    Maintscript.interp
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
            run_script ~cmd_line_arguments ~states ~package ~script
          in
          (success, error, make_ran_node ~incomplete:(incomplete<>[]) ())
        with
        | Colis.Internals.Errors.Unsupported (utility, msg) ->
          ([], [], make_ran_node ~unsupported:[utility, msg] ())
        | Colis.Internals.Errors.CpuTimeLimitExceeded ->
          ([], [], make_ran_node ~timeout:true ())
        | Colis.Internals.Errors.MemoryLimitExceeded ->
          ([], [], make_ran_node ~oomemory:true ())
        | exn ->
          ([], [], make_ran_node ~unexpected:[exn] ())
      in
      RunScript (ran_node, (script, cmd_line_arguments), run success on_success, run error on_error)
  in
  Colis.Internals.Options.cpu_time_limit := Sys.time () +. cpu_timeout;
  let root = Colis.Constraints.Var.fresh ~hint:"r" () in
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Colis.Constraints.Clause.true_sat_conj fhs in
  let disj = List.map Colis.Constraints.Clause.make_initial disj in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  run stas scenario
