val generate_and_write :
  start_time:float -> end_time:float ->
  ?prefix:(string * string list) list ->
  copy_static:bool ->
  Package.t ->
  (Scenarii.Name.t * (Scenario.ran_leaf, Scenario.ran_node) Scenario.t) list ->
  unit
