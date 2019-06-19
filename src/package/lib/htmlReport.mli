val generate_and_write :
  prefix:string ->
  Package.t ->
  (Scenario.name * (Scenario.ran_leaf, Scenario.ran_node) Scenario.t) list ->
  unit
