(** {1 Contents}

    module for handling Contents files *)

val scan: string -> unit
(** [scanfile filename] scans the Contents file with name [filename],
    and adds its contents to the internal table. *)

val print: unit -> unit
(** dumps the internal table to [stdout] *)

val get_files : string -> string list
(** [get_files package] returns a list of path of regular files in [package]. *)
