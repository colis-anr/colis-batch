let table = Contents.newtable ()
let load () = Contents.scan !Options.contents table
let get_files package = Contents.get_files table package
