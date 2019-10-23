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
  ?incomplete:bool -> ?timeout:bool -> ?oomemory:bool -> ?notconverted:bool ->
  ?unsupported:(string * string) list -> ?unexpected:exn list ->
  unit -> ran_node

val ran_node_incomplete : ran_node -> bool
val ran_node_timeout : ran_node -> bool
val ran_node_oomemory : ran_node -> bool
val ran_node_not_converted : ran_node -> bool
val ran_node_unsupported : ran_node -> bool
val ran_node_unexpected : ran_node -> bool

type ran = (ran_leaf, ran_node) t

val states : ran -> (Status.t * Colis.Symbolic.Semantics.state list) list

val pp_ran_as_dot : ?name:string -> Format.formatter -> ran -> unit

(** {2 Ran Scenario Summarized} *)

type ran_leaf_sum = int
type ran_sum = (ran_leaf_sum, ran_node) t

val summarize : ran -> ran_sum

val states_sum : ran_sum -> (Status.t * int) list

(** {2 Ran Scenarios Coverage} *)

type coverage = Complete | Partial of ran_node | Null of ran_node
val coverage : ('a, ran_node) t -> coverage
