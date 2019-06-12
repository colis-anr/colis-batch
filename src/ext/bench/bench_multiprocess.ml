let epf = Format.printf

let map_p ~workers f l =
   Lwt_main.run (Colis_ext.MultiProcess.map_p ~workers f l)

let pp_int_list fmt l =
  let open Format in
  pp_print_char fmt '[';
  pp_print_list
    ~pp_sep:(fun fmt () -> pp_print_string fmt "; ")
    pp_print_int
    fmt l;
  pp_print_char fmt ']'

let list_map_tail_rec f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: q -> aux ((f h) :: acc) q
  in
  aux [] l

let sleep_time = 0.001
let f i = i + 1

let () =
  epf "List.map (fun i -> Unix.sleepf %f; i + 1)@.@." sleep_time;
  epf "             ||-----------------------------------------------------------------------------------------||@.";
  epf "             ||                                        Workers                                          ||@.";
  epf "||-----------||---------|---------|---------|---------|---------|---------|---------|---------|---------||@.";
  epf "|| List size ||    seq. |       1 |       2 |       4 |       8 |      16 |      32 |      64 |     128 ||@.";
  epf "||-----------||---------|---------|---------|---------|---------|---------|---------|---------|---------||@.";
  for log_length = 0 to 10 do
    let length = 1 lsl (2 * log_length) in
    epf "|| %9d ||@?" length;
    let list = List.init length (fun i -> i) in
    let expected = list_map_tail_rec f list in

    let start = Unix.gettimeofday () in
    let output = list_map_tail_rec (fun i -> Unix.sleepf sleep_time; f i) list in
    (* let output = list_map_tail_rec (fun i -> f i) list in *)
    let end_ = Unix.gettimeofday () in
    assert (output = expected);
    let time_s = Format.sprintf "%f" (end_ -. start) in
    let time_s = if String.length time_s > 7 then String.sub time_s 0 7 else time_s in
    epf " %7s |@?" time_s;

    for log_workers = 0 to 7 do
      let workers = 1 lsl log_workers in

      let start = Unix.gettimeofday () in
      let output = map_p ~workers (fun i -> Unix.sleepf sleep_time; f i) list in
      (* let output = map_p ~workers (fun i -> f i) list in *)
      let end_ = Unix.gettimeofday () in
      if output <> expected then
        (
          epf "@.@.Expected: %a@.Got:      %a@.@." pp_int_list expected pp_int_list output;
          exit 1
        );
      let time_s = string_of_float (end_ -. start) in
      let time_s = if String.length time_s > 7 then String.sub time_s 0 7 else time_s in
      epf " %7s |@?" time_s
    done;
    epf "|@.";
    epf "||-----------||---------|---------|---------|---------|---------|---------|---------|---------|---------||@."
  done
