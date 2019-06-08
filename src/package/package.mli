type name = string
type version = string

type t

val name : t -> name
val version : t -> version

val maintscript : t -> Maintscript.Key.t -> Maintscript.t

val parse : string -> t
