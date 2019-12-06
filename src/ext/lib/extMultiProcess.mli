(** {1 Multi-Process Parallel Functions} *)

val map_p : workers:int -> ('a -> 'b) -> 'a list -> 'b list Lwt.t
val mapi_p : workers:int -> (int -> 'a -> 'b) -> 'a list -> 'b list Lwt.t

val iter_p : workers:int -> ('a -> unit) -> 'a list -> unit Lwt.t
val iteri_p : workers:int -> (int -> 'a -> unit) -> 'a list -> unit Lwt.t

(** {2 Delayed Versions}

   These versions take a delayed list as input. This is different from
   waiting for the promise to be resolved and then calling a
   non-delayed version as these delayed version first create the
   workers then wait for the promise to be resolve. This is
   particularily useful when the promise can create very space
   consuming data that are not needed in the forks. *)

val map_dp : workers:int -> ('a -> 'b) -> 'a list Lwt.t -> 'b list Lwt.t
val mapi_dp : workers:int -> (int -> 'a -> 'b) -> 'a list Lwt.t -> 'b list Lwt.t

val iter_dp : workers:int -> ('a -> unit) -> 'a list Lwt.t -> unit Lwt.t
val iteri_dp : workers:int -> (int -> 'a -> unit) -> 'a list Lwt.t -> unit Lwt.t
