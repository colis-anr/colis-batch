type ('a, 'b) t = ('a, 'b) result

let bind a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let (>>=) = bind

let is_ok = function
  | Ok _ -> true
  | _ -> false

let unwrap_ok = function
  | Ok e -> e
  | Error _ -> assert false

let unwrap_error = function
  | Ok _ -> assert false
  | Error e -> e
