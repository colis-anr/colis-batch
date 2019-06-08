include Filename

let rec concat_l = function
  | [] -> raise (Invalid_argument "ExtFilename.concat_l")
  | [e] -> e
  | e :: t -> Filename.concat e (concat_l t)
