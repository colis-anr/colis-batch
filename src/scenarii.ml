open Scenario

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

let removal = (* FIXME: remove files *)
  action
    ~action:(RunScript (Maintscript.Prerm, ["remove"]))
    ~on_success:(
      action
        ~action:(RunScript (Maintscript.Postrm, ["remove"]))
        ~on_success:(status ConfigFiles)
        ~on_error:(status HalfInstalled)
    )
    ~on_error:(
      action
        ~action:(RunScript (Maintscript.Postinst, ["abort-remove"]))
        ~on_success:(status Installed)
        ~on_error:(status FailedConfig)
    )

let all =
  [ Install, install ;
    Removal, removal ]
