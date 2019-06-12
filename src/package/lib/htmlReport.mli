val generate_and_write :
  prefix:string ->
  Package.t ->
  (Scenario.name * Scenario.ran Scenario.t) list ->
  unit
