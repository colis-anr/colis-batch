include List

let rec update_assoc key value = function
  | [] -> failwith "ExtList.update_assoc"
  | (key', _) :: rest when key = key' ->
    (key, value) :: rest
  | (key', value') :: rest ->
    (key', value') :: update_assoc key value rest

let rec map_filter f = function
  | [] -> []
  | h :: q ->
    match f h with
    | None -> map_filter f q
    | Some x -> x :: map_filter f q

let group compare l =
  let rec group curr_key curr_vals = function
    | [] -> [curr_key, List.rev curr_vals]
    | (key, val_) :: rest ->
      if compare key curr_key = 0 then
        group curr_key (val_ :: curr_vals) rest
      else
        (curr_key, List.rev curr_vals) :: group key [val_] rest
  in
  match l with
  | [] -> []
  | (key, val_) :: rest -> group key [val_] rest
