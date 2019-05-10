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