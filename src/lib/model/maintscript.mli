(* Key *)

module Key : sig
  type t = Preinst | Postinst | Prerm | Postrm
  [@@deriving yojson]

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  val from_string : string -> t option
  val from_string_exn : string -> t
  (** Same as [t_from_string] except it raises an exception. *)

  val all : t list
end

(* Maintscript *)

type t
[@@deriving yojson]

val key : t -> Key.t
val key_as_string : t -> string
val has_key : Key.t -> t -> bool

val parse : string -> t

type error =
  | ParsingErrored of string
  | ParsingRejected
  | ConversionErrored of string
  | ConversionRejected of string

val has_error : t -> bool
val error : t -> error option

val error_to_string : error -> string

val colis : t -> Colis.colis

val interp :
  cmd_line_arguments:string list ->
  states:Colis.Symbolic.Semantics.state list ->
  package_name:string ->
  t ->
  Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list