type name = Preinst | Postinst | Prerm | Postrm

let name_to_string = function
  | Preinst -> "preinst"
  | Postinst -> "postinst"
  | Prerm -> "prerm"
  | Postrm -> "postrm"

let name_from_string = function
  | "preinst" -> Some Preinst
  | "postinst" -> Some Postinst
  | "prerm" -> Some Prerm
  | "postrm" -> Some Postrm
  | _ -> None

let name_from_string_exn str =
  match name_from_string str with
  | Some name -> name
  | _ -> raise (Invalid_argument "Maintscript.name_from_string_exn")

let all_names = [Preinst; Postinst; Prerm; Postrm]
