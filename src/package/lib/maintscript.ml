module Key = struct
  type t = Preinst | Postinst | Prerm | Postrm

  let to_string = function
    | Preinst -> "preinst"
    | Postinst -> "postinst"
    | Prerm -> "prerm"
    | Postrm -> "postrm"

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
  | ParsingRejected
  | ConversionErrored of string
  | ConversionRejected of string

let error_to_string = function
  | ParsingErrored msg -> "parsing errored: " ^ msg
  | ParsingRejected -> "parsing rejected"
  | ConversionErrored msg -> "conversion errored: " ^ msg
  | ConversionRejected msg -> "conversion rejected: " ^ msg

type t =
  { key : Key.t ;
    content : (Morsmall.AST.program, error) result }

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
      | Colis.Errors.ConversionError msg -> Error (ConversionRejected msg)
      | exn -> Error (ConversionErrored (Printexc.to_string exn))
    with
    | Morsmall.SyntaxError _pos -> Error ParsingRejected
    | exn -> Error (ParsingErrored (Printexc.to_string exn))
  in
  { key; content }

let error m =
  match m.content with
  | Ok _ -> None
  | Error e -> Some e

let has_error m =
  error m <> None

let colis m =
  match m.content with
  | Ok shell ->
    Colis.convert_shell_file ~cmd_line_arguments:["DUM"; "MY"] shell
  | _ ->
    raise (Invalid_argument "Maintscript.colis")

let interp ~cpu_timeout ~cmd_line_arguments ~states ~package_name m =
  match m.content with
  | Ok shell ->
    (* Assertion: it works because we have converted before (with other cmd line
       arguments). *)
    let colis = Colis.convert_shell_file ~cmd_line_arguments shell in
    let sym_states =
      List.map
        (Colis.Symbolic.to_symbolic_state
           ~vars:[
             "DPKG_MAINTSCRIPT_NAME", (Key.to_string m.key) ;
             "DPKG_MAINTSCRIPT_PACKAGE", package_name ;
           ] (* FIXME *)
           ~arguments:cmd_line_arguments)
        states
    in
    Constraints_common.Log.cpu_time_limit := Some (Sys.time () +. cpu_timeout);
    Colis.Symbolic.interp_program
      ~loop_limit:200
      ~stack_size:200
      ~argument0:(Key.to_string m.key)
      sym_states colis
  | Error _ -> raise (Invalid_argument "Maintscript.interp")
