val generate_and_write :
  prefix:string ->
  Package.t ->
  (Scenarii.Name.t * (Scenario.ran_leaf, Scenario.ran_node) Scenario.t) list ->
  unit
