let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf

let soi = string_of_int
