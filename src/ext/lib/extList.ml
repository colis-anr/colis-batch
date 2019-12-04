include List

let rec update_assoc key update = function
  | [] ->
    (match update None with
     | None -> []
     | Some value -> [key, value])
  | (key', value') :: rest ->
    if key = key' then
      match update (Some value') with
      | None -> rest
      | Some value' -> (key', value') :: rest
    else
      (key', value') :: update_assoc key update rest

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

let rec sub start length = function
  | [] -> []
  | h :: q ->
    if start > 0 then
      sub (start - 1) length q
    else if length = 0 then
      h :: (sub 0 (length - 1) q)
    else
      []

let flat_map f l = map f l |> flatten

let rec bd = function
  | [] -> failwith "bd"
  | [_] -> []
  | h :: t -> h :: bd t

let rec ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: t -> ft t

let rec ft_opt = function
  | [] -> None
  | [e] -> Some e
  | _ :: t -> ft_opt t
