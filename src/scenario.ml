let fpf = Format.fprintf
let spf = Format.sprintf

type status =
  | Installed
  | FailedConfig
  | NotInstalled
  | HalfInstalled
  | ConfigFiles
  | Unpacked

let pp_status fmt status =
  fpf fmt "%s" (match status with
      | Installed -> "Installed"
      | FailedConfig -> "Failed-Config"
      | NotInstalled -> "Not-Installed"
      | HalfInstalled -> "Half-Installed"
      | ConfigFiles -> "Config-Files"
      | Unpacked -> "Unpacked")

type action =
  (* FIXME: unpack *)
  | RunScript of Maintscript.name * string list

let pp_action fmt = function
  | RunScript (script, args) ->
    fpf fmt "%s" (Maintscript.name_to_string script);
    List.iter (fpf fmt " %s") args

type 'a scenario =
  | Status of status
  | Action of action * 'a t * 'a t

and 'a t =
  { data : 'a ;
    scenario : 'a scenario }

let action ~action ~on_success ~on_error =
  { data = () ; scenario = Action (action, on_success, on_error) }

let status status =
  { data = () ; scenario = Status status }

let rec pp pp_a fmt sc =
  let pp_title fmt = function
    | Status status -> fpf fmt "Status: %a" pp_status status
    | Action (action, _, _) -> pp_action fmt action
  in
  pp_title fmt sc.scenario;
  fpf fmt "@\n@[%a@]" pp_a sc.data;
  match sc.scenario with
  | Status _ -> ()
  | Action (_, on_success, on_error) ->
    fpf fmt "@\n@\nS-- @[%a@]@\n@\nE-- @[%a@]"
      (pp pp_a) on_success
      (pp pp_a) on_error

type colis_state = Colis.Symbolic.Semantics.state

type ran =
  { states : colis_state list ;
    incomplete : colis_state list ;
    timeout : bool }

type name =
  | Install

let name_to_string = function
  | Install -> "install"

let pp_ran fmt ran =
  fpf fmt "(states before: %d%s%s)"
    (List.length ran.states)
    (let s = List.length ran.incomplete in
     if s = 0 then "" else spf "; %d incomplete" s)
    (if ran.timeout then "; timeout" else "")

let pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc =
  let rec pp_as_dot ?parent fmt sc =
    let n = Hashtbl.hash sc in
    (
      match sc.scenario with
      | Status status ->
        fpf fmt "%d [label=\"%a\",shape=none];@\n"
          n
          pp_status status
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
    (match List.length sc.data.incomplete with
     | 0 -> ()
     | l -> fpf fmt "|%d incomplete" l);
    (if sc.data.timeout then
       fpf fmt "|timeout")
  in
  let pp_edge_label fmt sc =
    match List.length sc.data.states with
    | 0 -> ()
    | l -> fpf fmt "\\n%d" l
  in
  pp_as_dot ~pp_action_label ~pp_edge_label ~name fmt sc
