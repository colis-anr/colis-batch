val pp_viz : Format.formatter -> string -> unit

val with_formatter_to_file :
  string list ->
  (Format.formatter -> 'a) -> 'a

val with_formatter_to_html_report :
  ?title:string ->
  ?highlight:bool ->
  ?viz:bool ->
  string list ->
  (Format.formatter -> 'a) -> 'a
