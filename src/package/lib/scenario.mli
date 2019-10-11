module Status : sig
  type t =
    | Installed
    | FailedConfig
    | NotInstalled
    | HalfInstalled
    | ConfigFiles
    | Unpacked
    | OSEF
    | NonIdempotent

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

(** {1 Scenario} *)

type ('leaf, 'node) t =
  | Status of 'leaf * Status.t
  | Unpack of 'node * ('leaf, 'node) t
  | RunScript of 'node * (Maintscript.Key.t * string list) * ('leaf, 'node) t * ('leaf, 'node) t
(** The general type of a scenario, that can hold decorations on leafs and
    nodes. *)

val all_status : ('leaf, 'node) t -> Status.t list
(** Returns the list of status of one scenario. *)

(** {2 Clean Scenario} *)

type clean = (unit, unit) t

val status : Status.t -> clean

val unpack : on_success:clean -> clean

val run_script :
  on_success:clean -> on_error:clean ->
  ?args:string list -> Maintscript.Key.t -> clean

val pp_clean_as_dot : ?name:string -> Format.formatter -> clean -> unit

(** {2 Ran Scenario} *)

type ran_leaf = Colis.Symbolic.Semantics.state list
type ran_node

val make_ran_node :
  ?incomplete:bool -> ?timeout:bool -> ?oomemory:bool ->
  ?unsupported:(string * string) -> ?unexpected:exn ->
  unit -> ran_node

type ran = (ran_leaf, ran_node) t

val states : ran -> (Status.t * Colis.Symbolic.Semantics.state list) list

val pp_ran_as_dot : ?name:string -> Format.formatter -> ran -> unit

(** {2 Ran Scenario Summarized} *)

type ran_leaf_sum = int
type ran_sum = (ran_leaf_sum, ran_node) t
