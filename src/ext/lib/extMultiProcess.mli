val map_p : workers:int -> ('a -> 'b) -> 'a list -> 'b list Lwt.t
val mapi_p : workers:int -> (int -> 'a -> 'b) -> 'a list -> 'b list Lwt.t

val iter_p : workers:int -> ('a -> unit) -> 'a list -> unit Lwt.t
val iteri_p : workers:int -> (int -> 'a -> unit) -> 'a list -> unit Lwt.t
