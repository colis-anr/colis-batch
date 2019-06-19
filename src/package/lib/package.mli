type name = string
type version = string

type t

val path : t -> string
val name : t -> name
val version : t -> version
val safe_name : t -> string

val maintscript : t -> Maintscript.Key.t -> Maintscript.t option
val maintscripts : t -> Maintscript.t list
val iter_maintscripts : (Maintscript.t -> unit) -> t -> unit

val parse : string -> t

val are_all_maintscripts_ok : t -> bool
