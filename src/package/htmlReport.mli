(* val pp_header : title:string -> Format.formatter -> unit -> unit
val pp_footer : Format.formatter -> unit -> unit *)

val with_formatter_to_file :
  ?relative:bool -> string list ->
  (Format.formatter -> 'a) -> 'a

val with_formatter_to_report :
  ?title:string ->
  ?highlight:bool ->
  ?viz:bool ->
  ?relative:bool -> string list ->
  (Format.formatter -> 'a) -> 'a

val pp_package_parsing_status : Format.formatter -> Package.t -> unit

module Package : sig
  (* val pp_parsing_status : Format.formatter -> string -> unit *)
  val pp_scenarii : Format.formatter -> string -> unit
end

module Scenario : sig
  val pp_package :
    Format.formatter ->
    package:string ->
    string ->
    (Scenario.Status.t * Colis.Symbolic.Semantics.state list) list ->
    unit

  val pp_state :
    Format.formatter ->
    package:string ->
    status:Scenario.Status.t ->
    id:int ->
    Colis.Symbolic.Semantics.state ->
    unit
end

module Script : sig
  val pp_content : Format.formatter -> package:string -> string -> unit

  val pp_accepted : Format.formatter -> Colis.colis -> unit
  val pp_conversion_rejected : Format.formatter -> string -> unit
  val pp_conversion_errored : Format.formatter -> string -> unit
  val pp_parsing_rejected : Format.formatter -> unit -> unit
  val pp_parsing_errored : Format.formatter -> string -> unit
end

val generate_and_write : unit -> unit
