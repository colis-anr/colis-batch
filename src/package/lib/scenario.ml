open Colis_ext

module Status = struct
  type t =
    | Installed
    | FailedConfig
    | NotInstalled
    | HalfInstalled
    | ConfigFiles
    | Unpacked
    | OSEF
    | NonIdempotent

  let to_string = function
    | Installed -> "Installed"
    | FailedConfig -> "Failed-Config"
    | NotInstalled -> "Not-Installed"
    | HalfInstalled -> "Half-Installed"
    | ConfigFiles -> "Config-Files"
    | Unpacked -> "Unpacked"
    | OSEF -> "OSEF"
    | NonIdempotent -> "Non-Idempotent"

  let pp fmt status =
    fpf fmt "%s" (to_string status)
end

(* ============================== [ Scenario ] ============================== *)

type ('leaf, 'node) t =
  | Status of
      'leaf
      * Status.t
  | Unpack of
      'node
      * ('leaf, 'node) t
  | RunScript of
      'node
      * (Maintscript.Key.t * string list)
      * ('leaf, 'node) t * ('leaf, 'node) t

let all_status sc =
  let rec status = function
    | Status (_, st) -> [st]
    | Unpack (_, sc) -> status sc
    | RunScript (_, _, sc1, sc2) -> status sc1 @ status sc2
  in
  status sc
  |> List.sort_uniq compare

let pp_as_dot ?(name="noideafixme") ~pp_leaf_decoration ~pp_node_decoration fmt sc =
  let pp_run_script fmt (script, args) =
    fpf fmt "%s" (Maintscript.Key.to_string script);
    List.iter (fpf fmt " %s") args
  in
  let hash = Hashtbl.hash in
  let rec pp_as_dot fmt sc =
    match sc with
    | Status (leaf_decoration, status) ->
      fpf fmt "%d [label=< <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD>%a</TD></TR>%a</TABLE> >];@\n"
        (hash sc)
        Status.pp status
        pp_leaf_decoration leaf_decoration
    | Unpack (node_decoration, on_success) ->
      fpf fmt "%d [label=< <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD><I>unpack</I></TD></TR>%a</TABLE> >];@\n"
        (hash sc)
        pp_node_decoration node_decoration;
      fpf fmt "%d -> %d;@\n"
        (hash sc) (hash on_success);
      fpf fmt "@\n@[<h 2>  %a@]@\n"
        pp_as_dot on_success

    | RunScript (node_decoration, script, on_success, on_error) ->
      fpf fmt "%d [label=< <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD>%a</TD></TR>%a</TABLE> >];@\n"
        (hash sc)
        pp_run_script script
        pp_node_decoration node_decoration;
      fpf fmt "%d -> %d [label=\"OK\"];@\n%d -> %d [label=\"Failed\"];@\n"
        (hash sc) (hash on_success) (hash sc) (hash on_error);
      fpf fmt "@\n@[<h 2>  %a@]@\n@[<h 2>  %a@]@\n"
        pp_as_dot on_success
        pp_as_dot on_error
  in
  fpf fmt "digraph %s {@\n@[<h 2>  node [shape=plaintext];@\n@\n%a@]}@."
    name pp_as_dot sc

(* =========================== [ Clean Scenario ] =========================== *)

type clean = (unit, unit) t

let status status = Status ((), status)

let unpack ~on_success =
  Unpack ((), on_success)

let run_script ~on_success ~on_error ?(args=[]) key =
  RunScript ((), (key, args), on_success, on_error)

let pp_clean_as_dot ?name fmt sc =
  let pp_nop _ _ = () in
  pp_as_dot ?name ~pp_leaf_decoration:pp_nop ~pp_node_decoration:pp_nop fmt sc

(* ============================ [ Ran Scenario ] ============================ *)

type ran_leaf = Colis.Symbolic.Semantics.state list

type ran_node =
  { incomplete : bool ;
    timeout : bool ;
    oomemory : bool ;
    unsupported : (string * string) option ;
    unexpected : exn option }

let make_ran_node
    ?(incomplete=false) ?(timeout=false) ?(oomemory=false)
    ?unsupported ?unexpected
    ()
  = { incomplete ; timeout ; oomemory ; unsupported ; unexpected }

let ran_node_had_problem r =
  r.incomplete || r.timeout || r.oomemory
  || r.unsupported <> None || r.unexpected <> None

type ran = (ran_leaf, ran_node) t

let states sc =
  let rec states = function
    | Status (states, st) -> List.map (fun state -> (st, state)) states
    | Unpack (_, sc) -> states sc
    | RunScript (_, _, s1, s2) -> states s1 @ states s2
  in
  states sc
  |> List.sort compare
  |> List.group compare

type coverage = Complete | Partial | Null

let merge_coverage a b =
  match a, b with
  | Null, Null -> Null
  | Complete, Complete -> Complete
  | _ -> Partial

let rec coverage = function
  | Status _ -> Complete
  | Unpack (r, sc) ->
    if ran_node_had_problem r then
      Null
    else
      coverage sc
  | RunScript (r, _, s1, s2) ->
    if ran_node_had_problem r then
      Null
    else
      merge_coverage (coverage s1) (coverage s2)

let pp_ran_as_dot ?name fmt sc =
  let pp_leaf_decoration fmt states =
    fpf fmt "<TR><TD>(%d states)</TD></TR>" (List.length states)
  in
  let pp_node_decoration fmt dec =
    if dec.incomplete then
      fpf fmt "<TR><TD>incomplete</TD></TR>";
    if dec.timeout then
      fpf fmt "<TR><TD>timeout</TD></TR>";
    if dec.oomemory then
      fpf fmt "<TR><TD>out of memory</TD></TR>";
    if dec.unsupported <> None then
      fpf fmt "<TR><TD>unsup. utility</TD></TR>";
    if dec.unexpected <> None then
      fpf fmt "<TR><TD>unexpected exception</TD></TR>"
  in
  pp_as_dot ?name ~pp_leaf_decoration ~pp_node_decoration fmt sc

(* ======================= [ Ran Scenario Summarized ] ====================== *)

type ran_leaf_sum = int
type ran_sum = (ran_leaf_sum, ran_node) t

let summarize_ran_leaf = List.length

let rec summarize = function
  | Status (leaf, st) -> Status (summarize_ran_leaf leaf, st)
  | Unpack (node, sc) -> Unpack (node, summarize sc)
  | RunScript (node, script, sc1, sc2) ->
    RunScript (node, script, summarize sc1, summarize sc2)

let states_sum sc =
  let rec states_sum = function
    | Status (states, st) -> [st, states]
    | Unpack (_, sc) -> states_sum sc
    | RunScript (_, _, s1, s2) -> states_sum s1 @ states_sum s2
  in
  states_sum sc
  |> List.sort compare
  |> List.group compare
  |> List.map (fun (sc, l) -> (sc, List.fold_left (+) 0 l))
