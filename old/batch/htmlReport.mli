open Colis_package

val generate_and_write :
  start_time:float -> end_time:float ->
  (Package.t * (Scenarii.Name.t * Scenario.ran_sum) list) list ->
  unit
