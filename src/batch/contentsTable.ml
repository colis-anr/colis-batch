let table = Contents.newtable ()
let load () = Contents.scan !Colis_config.contents table
let get_files package = Contents.get_files table package
