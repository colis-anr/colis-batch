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

let all =
  [ Install, install ]

let run ~package name =
  ScenarioEngine.run ~package ~name (List.assoc name all)
