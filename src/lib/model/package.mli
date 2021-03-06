type name = string
type version = string

type t [@@deriving yojson]

val path : t -> string
val name : t -> name
val version : t -> version
val content : t -> string list

val safe_name : t -> string

val has_maintscript : t -> Maintscript.Key.t -> bool
val maintscript : t -> Maintscript.Key.t -> Maintscript.t option

val maintscripts : t -> Maintscript.t list
val iter_maintscripts : (Maintscript.t -> unit) -> t -> unit

val parse_from_dir : string -> t

val are_all_maintscripts_ok : t -> bool
