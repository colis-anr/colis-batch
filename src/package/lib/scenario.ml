open Colis_ext

module Status = struct
  type t =
    | Installed
    | FailedConfig
    | NotInstalled
    | HalfInstalled
    | ConfigFiles
    | Unpacked

  let to_string = function
    | Installed -> "Installed"
    | FailedConfig -> "Failed-Config"
    | NotInstalled -> "Not-Installed"
    | HalfInstalled -> "Half-Installed"
    | ConfigFiles -> "Config-Files"
    | Unpacked -> "Unpacked"

  let pp fmt status =
    fpf fmt "%s" (to_string status)
end

type action =
  (* FIXME: unpack *)
  | RunScript of Maintscript.Key.t * string list

type 'a scenario =
  | Status of Status.t
  | Action of action * 'a t * 'a t

and 'a t =
  { data : 'a ;
    scenario : 'a scenario }

let action ~action ~on_success ~on_error =
  { data = () ; scenario = Action (action, on_success, on_error) }

let status status =
  { data = () ; scenario = Status status }

let all_status s =
  let rec status s =
    match s.scenario with
    | Status st -> [st]
    | Action (_, s1, s2) -> status s1 @ status s2
  in
  status s
  |> List.sort_uniq compare

type colis_state = Colis.Symbolic.Semantics.state

type ran =
  { states : colis_state list ;
    incomplete : bool ;
    timeout : bool ;
    unsupported_utility : (string * string) option ;
    unsupported_argument : (string * string * string) option }

let states s =
  let rec states s = (* FIXME: regroup by status; otherwise, reports will be broken *)
    match s.scenario with
    | Status st -> List.map (fun state -> (st, state)) s.data.states
    | Action (_, s1, s2) -> states s1 @ states s2
  in
  states s
  |> List.sort compare
  |> List.group compare

type name =
  | Install
  | Removal
  | RemovalPurge

let name_to_string = function
  | Install -> "install"
  | Removal -> "removal"
  | RemovalPurge -> "removal_purge"

let pp_action fmt = function
  | RunScript (script, args) ->
    fpf fmt "%s" (Maintscript.Key.to_string script);
    List.iter (fpf fmt " %s") args

let pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc =
  let rec pp_as_dot ?parent fmt sc =
    let n = Hashtbl.hash sc in
    (
      match sc.scenario with
      | Status status ->
        fpf fmt "%d [label=\"%a\",shape=none];@\n"
          n
          Status.pp status
      | Action (action, on_success, on_error) ->
        fpf fmt "%d [label=\"{%a%a}\"];@\n@\n@[<h 2>  %a@]@\n@[<h 2>  %a@]@\n"
          n
          pp_action action
          pp_action_label sc
          (pp_as_dot ~parent:("OK", n)) on_success
          (pp_as_dot ~parent:("Failed", n)) on_error
    );
    (
      match parent with
      | None -> ()
      | Some (kind, parent) ->
        fpf fmt "%d -> %d [label=\"%s%a\"];@\n"
          parent n kind
          pp_edge_label sc
    )
  in
  fpf fmt "digraph %s {@\n@[<h 2>  node[shape=Mrecord];@\n@\n%a@]}@."
    (name_to_string name) (pp_as_dot ?parent:None) sc

let pp_unit_as_dot ~name fmt sc =
  let pp_nop _ _ = () in
  pp_as_dot ~pp_action_label:pp_nop ~pp_edge_label:pp_nop ~name fmt sc

let pp_ran_as_dot ~name fmt sc =
  let pp_action_label fmt sc =
    if sc.data.incomplete then
      fpf fmt "|incomplete";
    if sc.data.timeout then
      fpf fmt "|timeout";
    (match sc.data.unsupported_utility with
     | None -> ()
     | Some (utility, _) -> fpf fmt "|unsup. util.: %s" utility);
    (match sc.data.unsupported_argument with
     | None -> ()
     | Some (utility, _, arg) -> fpf fmt "|bad arg. for %s: %s" utility arg)
  in
  let pp_edge_label fmt sc =
    fpf fmt "\\n%d" (List.length sc.data.states)
  in
  pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc
