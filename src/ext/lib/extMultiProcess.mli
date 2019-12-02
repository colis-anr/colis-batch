val map_p : workers:int -> ('a -> 'b) -> 'a list -> 'b list Lwt.t

val iter_p : workers:int -> ('a -> unit) -> 'a list -> unit Lwt.t
