type ('a, 'b) t = ('a, 'b) result

val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

val is_ok : ('a, 'b) t -> bool

val unwrap_ok : ('a, 'b) t -> 'a
val unwrap_error : ('a, 'b) t -> 'b
