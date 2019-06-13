open Colis_package

val generate_and_write :
  prefix:string ->
  time:float ->
  (Package.t * (Scenario.name * (Scenario.Status.t * int) list) list) list ->
  unit
