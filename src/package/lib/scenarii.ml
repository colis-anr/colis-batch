open Scenario

let install = (* FIXME: unpack *)
  action
    ~action:(RunScript (Maintscript.Key.Preinst, ["install"]))
    ~on_success:(
      action
        ~action:(RunScript (Maintscript.Key.Postinst, ["configure"])) (* FIXME: version *)
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )
    ~on_error:(
      action
        ~action:(RunScript (Maintscript.Key.Postrm, ["abort-install"]))
        ~on_success:(status NotInstalled)
        ~on_error:(status HalfInstalled)
    )

let removal = (* FIXME: remove files *)
  action
    ~action:(RunScript (Maintscript.Key.Prerm, ["remove"]))
    ~on_success:(
      action
        ~action:(RunScript (Maintscript.Key.Postrm, ["remove"]))
        ~on_success:(status ConfigFiles)
        ~on_error:(status HalfInstalled)
    )
    ~on_error:(
      action
        ~action:(RunScript (Maintscript.Key.Postinst, ["abort-remove"]))
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )

let removal_purge =
  action
    ~action:(RunScript (Maintscript.Key.Prerm, ["remove"]))
    ~on_success:(
      action
        ~action:(RunScript (Maintscript.Key.Postrm, ["remove"]))
        ~on_success:(
          action
            ~action:(RunScript (Maintscript.Key.Postrm, ["purge"]))
            ~on_success:(status NotInstalled)
            ~on_error:(status ConfigFiles)
        )
        ~on_error:(status HalfInstalled)
    )
    ~on_error:(
      action
        ~action:(RunScript (Maintscript.Key.Postinst, ["abort-remove"]))
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )

let all =
  [ Install, install ;
    Removal, removal ;
    RemovalPurge, removal_purge ]
