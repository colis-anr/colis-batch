open Colis_ext

let pp_package_parsing_status package =
  pf "Parsed:@\n- name: %s@\n- version: %s@\n- scripts:@."
    (Package.name package) (Package.version package);
  Package.iter_maintscripts
    (fun (key, maintscript) ->
       pf "  - %s: %s@."
         (Maintscript.Key.to_string key)
         (if Maintscript.is_present maintscript then
            (match Maintscript.has_error maintscript with
             | None -> "OK"
             | Some e -> Maintscript.error_to_string e)
          else
            "absent"))
    package;
