open Protocol_conv_jsonm

let (>>=) = Lwt.bind

(* Options *)

module Options = struct
  let timeout = ref 10.
  let colis_cmd = ref "colis"
  let workers = ref 8
  let report_template = ref Filename.(concat (dirname Sys.argv.(0)) "report.template.html")

  let html_output = ref "report.html"
  let json_output = ref "report.json"
end

(* Workers *)

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

let process_status_to_jsonm = function
  | WEXITED n -> `String ("Exited " ^ string_of_int n)
  | WSIGNALED n -> `String ("Signaled " ^ string_of_int n)
  | WSTOPPED n -> `String ("Stopped " ^ string_of_int n)

type report =
  { file : string ;
    status : process_status ;
    stdout : string ;
    stderr : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

let files = Queue.create ()
let reports = Queue.create ()

let report_from_process file process =
  process#status >>= fun status ->
  Lwt_io.read process#stdout >>= fun stdout ->
  Lwt_io.read process#stderr >>= fun stderr ->
  process#close >>= fun _ ->
  Lwt.return { file ; status ; stdout ; stderr }

let rec worker wid =
  try
    let file = Queue.pop files in
    Format.eprintf "[%d] %s \t(%d remaining)@." wid file (Queue.length files);
    let process =
      (!Options.colis_cmd,
       [|"--shell"; "--run-symbolic"; "--fail-on-unknown-utilities";
         file|])
    in
    Lwt.catch
      (fun () ->
        Lwt_process.with_process_full
          ~timeout:!Options.timeout
          process (report_from_process file)
        >>= fun report ->
        Queue.add report reports;
        Lwt.return ())
      (function
       | Lwt_io.Channel_closed _ ->
          (* Timeout where closing the channels was faster than
             receiving the SIGKILL signal. *)
          Queue.add { file ; status = WSIGNALED Sys.sigkill ; stdout = "" ; stderr = "" } reports;
          Lwt.return ()
       | _exn ->
          (* FIXME *)
          Lwt.return ())
    >>= fun () ->
    worker wid
  with
    Queue.Empty ->
    Lwt.return ()

let workers n =
  List.init n worker
  |> Lwt.join

(* Report *)

type report_list =
  { number : int ;
    percentage : int ;
    list : report list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let report_list_from_report_list ~total list =
  let number = List.length list in
  { number ; percentage = 100 * number / total ; list }

type parameters =
  { timeout : float }
[@@deriving to_protocol ~driver:(module Jsonm)]

type infos =
  { total : int }
[@@deriving to_protocol ~driver:(module Jsonm)]

type generic_report =
  { success : report_list ;
    error : report_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

type execution_report =
  { success : report_list ;
    error : report_list ;
    timeout : report_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

type full_report =
  { parameters : parameters ;
    infos : infos ;
    parsing : generic_report ;
    conversion : generic_report ;
    execution : execution_report ;
    verification : generic_report ;
    other_error : report_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let is_verification_success report = report.status = Unix.WEXITED 0
let is_verification_error report = report.status = Unix.WEXITED 1
let is_execution_timeout report = report.status = Unix.WSIGNALED Sys.sigkill
let is_execution_error report = report.status = Unix.WEXITED 7 || report.status = Unix.WEXITED 8
let is_conversion_error report = report.status = Unix.WEXITED 6
let is_parsing_error report = report.status = Unix.WEXITED 5
let is_other_error report =
  not (is_verification_success report || is_verification_error report
       || is_execution_timeout report || is_execution_error report
       || is_conversion_error report || is_parsing_error report)

let generate_report () =
  let reports = Queue.to_seq reports |> List.of_seq in
  let total = List.length reports in

  let parameters = { timeout = !Options.timeout } in

  let infos = { total } in

  let other_error, reports =
    List.partition is_other_error reports
  in
  let other_error = report_list_from_report_list ~total other_error in

  let parsing_error, reports =
    List.partition is_parsing_error reports
  in
  let parsing =
    { success = report_list_from_report_list ~total reports ;
      error = report_list_from_report_list ~total parsing_error }
  in

  let conversion_error, reports =
    List.partition is_conversion_error reports
  in
  let conversion =
    { success = report_list_from_report_list ~total reports ;
      error = report_list_from_report_list ~total conversion_error }
  in

  let execution_error, reports =
    List.partition is_execution_error reports
  in
  let execution_timeout, reports =
    List.partition is_execution_timeout reports
  in
  let execution =
    { success = report_list_from_report_list ~total reports ;
      timeout = report_list_from_report_list ~total execution_timeout ;
      error = report_list_from_report_list ~total execution_error }
  in

  let verification_error, reports =
    List.partition is_verification_error reports
  in
  let verification_success, reports =
    List.partition is_verification_success reports
  in
  assert (reports = []);
  let verification =
    { success = report_list_from_report_list ~total verification_success ;
      error = report_list_from_report_list ~total verification_error }
  in

  { parameters ; infos ; parsing ; conversion ; execution ; verification ; other_error }

let report_to_json report =
  match full_report_to_jsonm report with
  | `O json -> `O json
  | _ -> assert false

let render_json_report ~channel json =
  output_string channel (Ezjsonm.to_string ~minify:false json)

let render_html_report ~channel json =
  let template =
    let ichan = open_in !Options.report_template in
    let template = ichan |> Lexing.from_channel |> Mustache.parse_lx in
    close_in ichan;
    template
  in
  let fmt =
    (* FIXME: output file *)
    Format.formatter_of_out_channel channel
  in
  Mustache.render_fmt fmt template json;
  Format.pp_print_flush fmt ()

let main () =
  Lwt_io.(read_lines stdin)
  |> Lwt_stream.iter (fun file -> Queue.add file files) >>= fun () ->
  workers !Options.workers >>= fun () ->
  let json = generate_report () |> report_to_json in
  let () =
    let channel = open_out !Options.json_output in
    render_json_report ~channel json;
    close_out channel
  in
  let () =
    let channel = open_out !Options.html_output in
    render_html_report ~channel json;
    close_out channel
  in
  Lwt_io.eprint "Done!\n"

let () =
  Lwt_main.run (main ())
