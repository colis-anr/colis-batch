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
  | ParsingRejected of Lexing.position
  | ConversionErrored of string
  | ConversionRejected of (Morsmall.Location.position * string)

val has_error : t -> bool
val error : t -> error option

val error_to_string : error -> string

val colis : ?cmd_line_arguments:string list -> t -> Colis.colis
(** Returns the scriprt as Colis, using the given command line
   arguments, or dummy ones if not provided. Raises [Failure
   "Maintscript.colis"] if the script cannot be translated. *)

val utilities : t -> (string * (string list * int) list) list
(** List of utilities called by the script, with, for each one of
   them, a list of abstractions of the ways they are called (with a
   list of dashed arguments and the number of non-dashed ones).
   Raises [Failure "Maintscript.colis"] if the script cannot be
   translated. *)

val interp :
  cmd_line_arguments:string list ->
  states:Colis.Symbolic.Semantics.state list ->
  package_name:string ->
  t ->
  Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list
  * Colis.Symbolic.Semantics.state list
(** Interprets the script. Raises [Failure "Maintscript.colis"] if the
   script cannot be translated. *)
