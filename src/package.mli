type name = string

type t

val name : t -> name
val maintscripts : t -> (Maintscript.name * Colis.colis option) list

val parse : name:string -> t option
(** Parses the package. If everything goes fine, returns the package. Otherwise,
    returns [None] and informs the [Stats] modules. *)
