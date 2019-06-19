include (module type of List)

val update_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val group : ('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list

val map_filter : ('a -> 'b option) -> 'a list -> 'b list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list

val sub : int -> int -> 'a list -> 'a list
