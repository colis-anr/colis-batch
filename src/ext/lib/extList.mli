include (module type of List)

val update_assoc : 'a -> ('b option -> 'b option) -> ('a * 'b) list -> ('a * 'b) list

val group : ('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list

val map_filter : ('a -> 'b option) -> 'a list -> 'b list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list

val sub : int -> int -> 'a list -> 'a list

val bd : 'a list -> 'a list
(** Returns the list without its last element. *)

val ft : 'a list -> 'a

val ft_opt : 'a list -> 'a option
(** Returns the last element of a list, or [None] if the list is empty. *)
