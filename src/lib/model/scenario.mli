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
  [@@deriving yojson]

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
[@@deriving yojson]

val status : Status.t -> clean

val unpack : on_success:clean -> clean

val run_script :
  on_success:clean -> on_error:clean ->
  ?args:string list -> Maintscript.Key.t -> clean

val pp_clean_as_dot : ?name:string -> Format.formatter -> clean -> unit

(** {2 Ran Scenario} *)

type 'a ran_node_gen

val ran_node_gen_incomplete : 'a ran_node_gen -> bool
val ran_node_gen_timeout : 'a ran_node_gen -> bool
val ran_node_gen_oomemory : 'a ran_node_gen -> bool
val ran_node_gen_notconverted : 'a ran_node_gen -> bool
val ran_node_gen_unsupported : 'a ran_node_gen -> (string * string) list
val ran_node_gen_has_unsupported : 'a ran_node_gen -> bool
val ran_node_gen_unexpected : 'a ran_node_gen -> string list
val ran_node_gen_has_unexpected : 'a ran_node_gen -> bool

type ran_leaf = Colis.Symbolic.Semantics.state list
type ran_node = Colis.Symbolic.Semantics.state list ran_node_gen

val make_ran_node :
  ?absent:bool -> ?incomplete:bool -> ?timeout:bool -> ?oomemory:bool -> ?notconverted:bool ->
  ?unsupported:(string * string) list -> ?unexpected:exn list ->
  Colis.Symbolic.Semantics.state list -> ran_node

val ran_node_incomplete : ran_node -> bool
val ran_node_timeout : ran_node -> bool
val ran_node_oomemory : ran_node -> bool
val ran_node_notconverted : ran_node -> bool
val ran_node_unsupported : ran_node -> (string * string) list
val ran_node_has_unsupported : ran_node -> bool
val ran_node_unexpected : ran_node -> string list
val ran_node_has_unexpected : ran_node -> bool

type ran = (ran_leaf, ran_node) t
[@@deriving yojson]

val states : ran -> (Status.t * Colis.Symbolic.Semantics.state list) list

val pp_ran_as_dot : ?name:string -> Format.formatter -> ran -> unit

(** {2 Ran Scenario Summarized} *)

type ran_leaf_sum = int

type ran_node_sum = int ran_node_gen

type ran_sum = (ran_leaf_sum, ran_node_sum) t
[@@deriving yojson]

val summarize : ran -> ran_sum

val states_sum : ran_sum -> (Status.t * int) list

type 'a coverage = Complete | Partial of 'a ran_node_gen | Null of 'a ran_node_gen

val coverage : ('a, 'b ran_node_gen) t -> 'b coverage
