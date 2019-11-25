let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf
let aspf = Format.asprintf

let soi = string_of_int
let iof = int_of_float
let foi = float_of_int

let fnot f x = not (f x)

let significant ~precision n =
  let l = 10. ** ((foi precision) -. 1. -. floor (log10 n)) in
  (floor ((n *. l) +. 0.5)) /. l
