(* Key *)

module Key : sig
  type t = Preinst | Postinst | Prerm | Postrm

  val to_string : t -> string
  val from_string : string -> t option
  val from_string_exn : string -> t
  (** Same as [t_from_string] except it raises an exception. *)

  val all : t list
end

(* Maintscript *)

type t

val parse : string -> t

type error =
  | ParsingErrored of string
  | ParsingRejected
  | ConversionErrored of string
  | ConversionRejected of string

val error : t -> error option

val interp :
  cmd_line_arguments:string list ->
  states:Colis.Symbolic.Semantics.state list ->
  key:Key.t ->
  t ->
  Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list
