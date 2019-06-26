open Colis_package

val generate_and_write :
  time:float ->
  (Package.t * (Scenarii.Name.t * (Scenario.Status.t * int) list) list) list ->
  unit
