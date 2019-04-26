type name = Preinst | Postinst | Prerm | Postrm

val name_to_string : name -> string
val name_from_string : string -> name option
val name_from_string_exn : string -> name
(** Same as [name_from_string] except it raises an exception. *)

val all_names : name list
