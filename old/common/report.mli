val pp_viz : Format.formatter -> string -> unit

val with_formatter_to_file :
  string list ->
  (Format.formatter -> 'a) -> 'a

val with_formatter_to_html_report :
  ?highlight:bool ->
  ?viz:bool ->
  (string * string list) list ->
  (Format.formatter -> 'a) -> 'a

val copy_static_to : string list -> unit
