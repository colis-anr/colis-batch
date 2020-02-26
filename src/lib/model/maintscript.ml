open Colis_batch_ext

module Key = struct
  type t = Preinst | Postinst | Prerm | Postrm
  [@@deriving yojson]

  let to_string = function
    | Preinst -> "preinst"
    | Postinst -> "postinst"
    | Prerm -> "prerm"
    | Postrm -> "postrm"

  let pp fmt s =
    fpf fmt "%s" (to_string s)

  let from_string = function
    | "preinst" -> Some Preinst
    | "postinst" -> Some Postinst
    | "prerm" -> Some Prerm
    | "postrm" -> Some Postrm
    | _ -> None

  let from_string_exn str =
    match from_string str with
    | Some key -> key
    | _ -> raise (Invalid_argument "Maintscript.key_from_string_exn")

  let all = [Preinst; Postinst; Prerm; Postrm]
end

type error =
  | ParsingErrored of string
  | ParsingRejected of Morsmall.Location.lexing_position (* = Lexing.position but Morsmall includes yojson (de)serialisers *)
  | ConversionErrored of string
  | ConversionRejected of (Morsmall.Location.position * string)
[@@deriving yojson]

let error_to_string = function
  | ParsingErrored msg -> "parsing errored: " ^ msg
  | ParsingRejected _pos -> "parsing rejected" (* FIXME: pos *)
  | ConversionErrored msg -> "conversion errored: " ^ msg
  | ConversionRejected (_pos, msg) -> "conversion rejected: " ^ msg (* FIXME: pos *)

type t =
  { key : Key.t ;
    content : (Morsmall.AST.program, error) Serialisable.s_result }
[@@deriving yojson]

let key m = m.key
let key_as_string m = Key.to_string m.key
let has_key k m = m.key = k

let parse path =
  let key = Key.from_string_exn (Filename.basename path) in
  let content =
    try
      let shell = Morsmall.parse_file path in
      try
        (* We try to convert with dummy arguments to see if we really can. *)
        (* FIXME: we need a better way than parsing again and again. *)
        let _colis = Colis.Language.FromShell.program__to__program ~cmd_line_arguments:["DUM"; "MY"] shell in
        Ok shell
      with
      | Colis.Internals.Errors.ConversionError (pos, msg) -> Error (ConversionRejected (pos, msg))
      | exn -> Error (ConversionErrored (Printexc.to_string exn))
    with
    | Morsmall.SyntaxError pos -> Error (ParsingRejected pos)
    | exn -> Error (ParsingErrored (Printexc.to_string exn))
  in
  { key; content }

let error m =
  match m.content with
  | Ok _ -> None
  | Error e -> Some e

let has_error m =
  error m <> None

let colis ?(cmd_line_arguments=["DUM"; "MY"]) m =
  match m.content with
  | Ok shell ->
    Colis.Language.convert_shell_file ~cmd_line_arguments shell
    |> Colis.Language.embellish_colis
  | _ ->
    raise (Invalid_argument "Maintscript.colis")

let utilities script =
  let abstract_args args =
    let (opts, args) =
      args
      |> List.map fst
      |> List.partition
        (function
          | Colis.Language.Syntax.SLiteral arg
            when String.length arg > 0 && arg.[0] = '-' && String.index_opt arg ' ' = None ->
            true
          | _ -> false)
    in
    let opts =
      opts
      |> List.map
        (function
          | Colis.Language.Syntax.SLiteral arg -> arg
          | _ -> assert false)
      |> List.sort String.compare
    in
    (opts, List.length args)
  in
  let visitor = object
    inherit [_] Colis.Language.SyntaxHelpers.reduce

    method zero = []
    method plus = (@)

    method! visit_ICallUtility () name args =
      [name, abstract_args args]
  end in
  visitor#visit_program () (colis script)
  |> List.group String.compare (* group by first argument *)
(* FIXME: group second argument and give list of positions *)

let interp ~cmd_line_arguments ~states ~package_name m =
  let colis = colis ~cmd_line_arguments m in
  let sym_states =
    List.map
      (Colis.SymbolicConstraints.to_symbolic_state
         ~vars:[
           "DPKG_MAINTSCRIPT_NAME", (Key.to_string m.key) ;
           "DPKG_MAINTSCRIPT_PACKAGE", package_name ;
         ] (* FIXME *)
         ~arguments:cmd_line_arguments)
      states
  in
  Colis.SymbolicConstraints.interp_program
    ~loop_limit:200
    ~stack_size:200
    ~argument0:(Key.to_string m.key)
    sym_states colis
