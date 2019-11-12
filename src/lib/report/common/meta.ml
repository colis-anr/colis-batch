type t =
  { start_time : float ;
    end_time : float ;
    duration : float }
[@@deriving yojson]

let make ~start_time ~end_time =
  let duration = floor (0.5 +. end_time -. start_time) in
  { start_time ; end_time ; duration }

let while_gathering_meta f =
  let start_time = Unix.gettimeofday () in
  let v = f () in
  let end_time = Unix.gettimeofday () in
  (make ~start_time ~end_time, v)
