open Colis_batch_ext
open Scenario

module Name = struct
  type t =
    | Install
    | Removal
    | RemovalPurge
    | Idempotency of Maintscript.Key.t * string list

  let to_string = function
    | Install -> "install"
    | Removal -> "removal"
    | RemovalPurge -> "removal_purge"
    | Idempotency (key, args) ->
      aspf "id_%a_%a"
        Maintscript.Key.pp key
        Format.(pp_print_list ~pp_sep:(fun fmt () -> fpf fmt "_") pp_print_string) args

  let to_fancy_string = function
    | Install -> "Installation"
    | Removal -> "Removal"
    | RemovalPurge -> "Removal and Purge"
    | Idempotency (key, args) ->
      aspf "Idempotency of %a %a"
        Maintscript.Key.pp key
        Format.(pp_print_list ~pp_sep:(fun fmt () -> fpf fmt " ") pp_print_string) args
end

let install = (* FIXME: unpack *)
  run_script
    Maintscript.Key.Preinst ~args:["install"]
    ~on_success:(
      unpack
        ~on_success:(
          run_script
            Maintscript.Key.Postinst ~args:["configure"] (* FIXME: version *)
            ~on_success:(status Installed)
            ~on_error:(status FailedConfig)
        )
    )
    ~on_error:(
      run_script
        Maintscript.Key.Postrm ~args:["abort-install"]
        ~on_success:(status NotInstalled)
        ~on_error:(status HalfInstalled)
    )

let removal = (* FIXME: remove files *)
  run_script
    Maintscript.Key.Prerm ~args:["remove"]
    ~on_success:(
      run_script
        Maintscript.Key.Postrm ~args:["remove"]
        ~on_success:(status ConfigFiles)
        ~on_error:(status HalfInstalled)
    )
    ~on_error:(
      run_script
        Maintscript.Key.Postinst ~args:["abort-remove"]
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )

let removal_purge =
  run_script
    Maintscript.Key.Prerm ~args:["remove"]
    ~on_success:(
      run_script
        Maintscript.Key.Postrm ~args:["remove"]
        ~on_success:(
          run_script
            Maintscript.Key.Postrm ~args:["purge"]
            ~on_success:(status NotInstalled)
            ~on_error:(status ConfigFiles)
        )
        ~on_error:(status HalfInstalled)
    )
    ~on_error:(
      run_script
        Maintscript.Key.Postinst ~args:["abort-remove"]
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )

let idempotency key args =
  run_script
    key ~args
    ~on_success:
      (
        run_script
          key ~args
          ~on_success:(status OSEF)
          ~on_error:(status NonIdempotent)
      )
    ~on_error:
      (
        run_script
          key ~args
          ~on_success:(status NonIdempotent)
          ~on_error:(status OSEF)
      )

let all =
  Name.[
    Install, install ;
    Removal, removal ;
    RemovalPurge, removal_purge ;
    Idempotency (Maintscript.Key.Preinst, ["install"]), idempotency Maintscript.Key.Preinst ["install"] ;
    Idempotency (Maintscript.Key.Postinst, ["configure"]), idempotency Maintscript.Key.Postinst ["configure"] ;
    Idempotency (Maintscript.Key.Postinst, ["abort-remove"]), idempotency Maintscript.Key.Postinst ["abort-remove"] ;
    Idempotency (Maintscript.Key.Prerm, ["remove"]), idempotency Maintscript.Key.Prerm ["remove"] ;
    Idempotency (Maintscript.Key.Postrm, ["remove"]), idempotency Maintscript.Key.Postrm ["remove"] ;
    Idempotency (Maintscript.Key.Postrm, ["purge"]), idempotency Maintscript.Key.Postrm ["purge"] ;
  ]
