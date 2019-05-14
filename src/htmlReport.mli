(* val pp_header : title:string -> Format.formatter -> unit -> unit
val pp_footer : Format.formatter -> unit -> unit *)

val with_formatter_to_report :
  ?title:string ->
  string -> (Format.formatter -> 'a) -> 'a

module Package : sig
  val pp_parsing_status : Format.formatter -> string -> unit
  val pp_scenarii : Format.formatter -> string -> unit
end

module Script : sig
  val pp_content : Format.formatter -> package:string -> string -> unit

  val pp_accepted : Format.formatter -> unit -> unit
  val pp_conversion_rejected : Format.formatter -> string -> unit
  val pp_conversion_errored : Format.formatter -> string -> unit
  val pp_parsing_rejected : Format.formatter -> unit -> unit
  val pp_parsing_errored : Format.formatter -> string -> unit
end

val generate_and_write : unit -> unit
