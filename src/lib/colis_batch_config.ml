type t =
  { workers : int ;
    cpu_timeout : float ;
    memory_limit : string ;
    report : string ;
    external_sources : string }
[@@deriving yojson]

let default =
  { workers = 2 ;
    cpu_timeout = 60. ;
    memory_limit = "1G" ;
    report = "report" ;
    external_sources = "external_sources" }
