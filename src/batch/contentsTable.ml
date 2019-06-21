let table = Contents.newtable ()

let load () =
  List.iter
    (fun content -> Contents.scan content table)
    !Colis_common.Config.contents

let get_files package = Contents.get_files table package
