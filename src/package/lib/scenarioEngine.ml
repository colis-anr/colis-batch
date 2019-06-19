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

let run_script ~cmd_line_arguments ~states ~package ~script =
  Maintscript.interp
    ~cmd_line_arguments
    ~states
    ~package_name:(Package.name package)
    ~key:script
    (Package.maintscript package script)

let run ~cpu_timeout ~package scenario =
  let rec run states (scenario : unit t) : ran t =
    match scenario.scenario with
    | Status status ->
      { scenario = Status status ;
        data = make_ran states }
    | Action (action, on_success, on_error) ->
      match action with
      | RunScript (script, cmd_line_arguments) ->
        let (success, error, data) =
          try
            let (success, error, incomplete) =
              run_script ~cpu_timeout ~cmd_line_arguments ~states ~package ~script
            in
            (success, error, make_ran ~incomplete:(incomplete<>[]) states)
          with
          | Colis.Errors.Unsupported (utility, msg) ->
            ([], [], make_ran ~unsupported:(utility, msg) states)
          | Constraints_common.Log.CPU_time_limit_exceeded ->
            ([], [], make_ran ~timeout:true states)
          | Constraints_implementation_efficient_clause.Safe.NotImplemented feature ->
            ([], [], make_ran ~not_implemented:feature states)
          | exn ->
            ([], [], make_ran ~unexpected:exn states)
        in
        { scenario = Action (action, run success on_success, run error on_error) ; data }
  in
  let root = Constraints.Var.fresh ~hint:"r" () in
  let disj = Colis.Symbolic.add_fs_spec_to_clause root Constraints.Clause.true_sat_conj fhs in
  let stas = List.map (Colis.Symbolic.to_state ~prune_init_state:false ~root) disj in
  run stas scenario
