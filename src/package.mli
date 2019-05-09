type name = string

type t

val name : t -> name
val maintscripts : t -> (Maintscript.name * Morsmall.AST.program option) list
val maintscript : t -> Maintscript.name -> Morsmall.AST.program option

val parse : string -> t option
(** Parses the package. If everything goes fine, returns the package. Otherwise,
    returns [None] and informs the [Stats] modules. *)

type status =
  | Installed
  | FailedConfig
  | NotInstalled
  | HalfInstalled
  | ConfigFiles
  | Unpacked
