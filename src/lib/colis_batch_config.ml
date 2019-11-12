type t =
  { workers : int ;
    cpu_timeout : float ;
    memory_limit : string }
[@@deriving yojson]

let default =
  { workers = 2 ;
    cpu_timeout = 60. ;
    memory_limit = "1G" }
