type name = string
type version = string

type t

val path : t -> string
val name : t -> name
val version : t -> version

val maintscript : t -> Maintscript.Key.t -> Maintscript.t
val iter_maintscripts : (Maintscript.Key.t * Maintscript.t -> unit) -> t -> unit

val parse : string -> t
