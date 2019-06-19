open Scenario

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

let all =
  [ Install, install ;
    Removal, removal ;
    RemovalPurge, removal_purge ]
