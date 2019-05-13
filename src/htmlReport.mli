val pp_header : title:string -> Format.formatter -> unit -> unit
val pp_footer : Format.formatter -> unit -> unit

module Package : sig
  val pp_parsing_status : Format.formatter -> string -> unit
  val pp_scenarii : Format.formatter -> string -> unit
end

module Script : sig
  val pp_content : Format.formatter -> string -> string -> unit
end

val generate_and_write : unit -> unit
