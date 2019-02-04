open Protocol_conv_jsonm

let (>>=) = Lwt.bind

(* Options *)

module Options = struct
  let timeout = ref 10.
  let colis_cmd = ref "colis"
  let workers = ref 8

  let template_prefix = ref "share/template"
  let report_prefix = ref "report"
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
    Format.eprintf "[worker: %2d; remaining files: %d] %s@." wid (Queue.length files) file;
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

type file =
  { file : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

type reports_group =
  { id : int ;
    files : file list ;
    number : int ;
    status : process_status ;
    short_stdout : string ;
    stdout : string ;
    short_stderr : string ;
    stderr : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

let reports_groups_list_from_reports_list reports =
  let shorten_out s =
    match String.split_on_char '\n' s with
    | [] -> ""
    | [e] | [e;""] -> e
    | e :: _ -> e ^ "\n[...]"
  in
  let compare_group (r1 : report) (r2 : report) =
    let c = compare r1.status r2.status in
    if c <> 0 then c
    else
      let c = compare r1.stdout r2.stdout in
      if c <> 0 then c
      else
        let c = compare r1.stderr r2.stderr in
        c
  in
  let add_to_group (report : report) (group : reports_group) =
    { group with files = { file = report.file } :: group.files ; number = group.number + 1 }
  in
  let rec group_aux (prev : report) curr = function
    | [] ->
       [curr]
    | h :: q ->
       if compare_group prev h = 0 then
         group_aux prev (add_to_group h curr) q
       else
         curr :: group q
  and group = function
    | [] -> []
    | h :: q -> group_aux
                  h
                  { id = 0 ;
                    files = [{ file = (h : report).file }] ; number = 1 ; status = h.status ;
                    short_stdout = shorten_out h.stdout ; stdout = h.stdout ;
                    short_stderr = shorten_out h.stderr ; stderr = h.stderr }
                  q
  in
  List.sort compare_group reports
  |> group
  |> List.map (fun g -> { g with id = Hashtbl.hash g.files })
  |> List.sort (fun g1 g2 -> - compare (List.length g1.files) (List.length g2.files))

type reports_groups_list =
  { number : int ;
    number_of_groups : int ;
    percentage_total : float ;
    percentage_previous : float ;
    groups : reports_group list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let report_list_from_report_list ~total ~previous reports =
  let number = List.length reports in
  let percentage_total = floor (10000. *. (float number) /. (float total)) /. 100. in
  let percentage_previous = floor (10000. *. (float number) /. (float previous)) /. 100. in
  let groups = reports_groups_list_from_reports_list reports in
  { number ; percentage_total ; percentage_previous ; number_of_groups = List.length groups ; groups }

type parameters =
  { timeout : float }
[@@deriving to_protocol ~driver:(module Jsonm)]

type infos =
  { total : int ;
    duration : float ;
    workers : int }
[@@deriving to_protocol ~driver:(module Jsonm)]

type generic_report =
  { success : reports_groups_list ;
    error : reports_groups_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

type execution_report =
  { success : reports_groups_list ;
    error : reports_groups_list ;
    timeout : reports_groups_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

type verification_report =
  { success : reports_groups_list ;
    error : reports_groups_list ;
    incomplete : reports_groups_list }
[@@deriving to_protocol ~driver:(module Jsonm)]

type full_report =
  { parameters : parameters ;
    infos : infos ;
    parsing : generic_report ;
    conversion : generic_report ;
    execution : execution_report ;
    verification : verification_report ;
    other_error : reports_groups_list ;
    all : reports_groups_list }
    [@@deriving to_protocol ~driver:(module Jsonm)]

let is_verification_success (report : report) = report.status = Unix.WEXITED 0
let is_verification_error (report : report) = report.status = Unix.WEXITED 10
let is_verification_error (report : report) = report.status = Unix.WEXITED 1
let is_execution_timeout (report : report) = report.status = Unix.WSIGNALED Sys.sigkill
let is_execution_error (report : report) = report.status = Unix.WEXITED 7 || report.status = Unix.WEXITED 8
let is_conversion_error (report : report) = report.status = Unix.WEXITED 6
let is_parsing_error (report : report) = report.status = Unix.WEXITED 5
let is_other_error (report : report) =
  not (is_verification_success report || is_verification_error report
       || is_execution_timeout report || is_execution_error report
       || is_conversion_error report || is_parsing_error report)

let generate_report ~duration () =
  let reports = Queue.to_seq reports |> List.of_seq in
  let total = List.length reports in
  let all = report_list_from_report_list ~total ~previous:total reports in

  let parameters = { timeout = !Options.timeout } in

  let infos = { total ; duration ; workers = !Options.workers } in

  let other_error, reports =
    List.partition is_other_error reports
  in
  let other_error = report_list_from_report_list ~total ~previous:total other_error in
  let previous = List.length reports in

  let parsing_error, reports =
    List.partition is_parsing_error reports
  in
  let parsing =
    { success = report_list_from_report_list ~total ~previous reports ;
      error = report_list_from_report_list ~total ~previous parsing_error }
  in
  let previous = List.length reports in

  let conversion_error, reports =
    List.partition is_conversion_error reports
  in
  let conversion =
    { success = report_list_from_report_list ~total ~previous reports ;
      error = report_list_from_report_list ~total ~previous conversion_error }
  in
  let previous = List.length reports in

  let execution_error, reports =
    List.partition is_execution_error reports
  in
  let execution_timeout, reports =
    List.partition is_execution_timeout reports
  in
  let execution =
    { success = report_list_from_report_list ~total ~previous reports ;
      timeout = report_list_from_report_list ~total ~previous execution_timeout ;
      error = report_list_from_report_list ~total ~previous execution_error }
  in
  let previous = List.length reports in

  let verification_error, reports =
    List.partition is_verification_error reports
  in
  let verification_incomplete, reports =
    List.partition is_verification_incomplete reports
  in
  let verification_success, reports =
    List.partition is_verification_success reports
  in
  assert (reports = []);
  let verification =
    { success = report_list_from_report_list ~total ~previous verification_success ;
      error = report_list_from_report_list ~total ~previous verification_error ;
      incomplete = report_list_from_report_list ~total ~previous verification_incomplete }
  in

  { parameters ; infos ; parsing ; conversion ; execution ; verification ; other_error ; all }

let full_report_to_json full_report =
  match full_report_to_jsonm full_report with
  | `O json -> `O json
  | _ -> assert false

let reports_group_to_json reports_group =
  match reports_group_to_jsonm reports_group with
  | `O json -> `O json
  | _ -> assert false

module Report = struct
  let render_template ~template ~output ~json =
    let template = Filename.concat !Options.template_prefix (template ^ ".html") in
    let output = Filename.concat !Options.report_prefix (output ^ ".html") in
    let template =
      let ichan = open_in template in
      let template = ichan |> Lexing.from_channel |> Mustache.parse_lx in
      close_in ichan;
      template
    in
    let ochan = open_out output in
    let fmt = Format.formatter_of_out_channel ochan in
    Mustache.render_fmt fmt template json;
    Format.pp_print_flush fmt ();
    close_out ochan

  let render_json json =
    let output = Filename.concat !Options.report_prefix "report.json" in
    let ochan = open_out output in
    output_string ochan (Ezjsonm.to_string ~minify:false json);
    close_out ochan

  let render_index json =
    render_template ~template:"index" ~output:"index" ~json

  let render_reports_group reports_group =
    let json = reports_group_to_json reports_group in
    render_template ~template:"group" ~output:("group-" ^ (string_of_int reports_group.id)) ~json

  let render report =
    let json = full_report_to_json report in
    render_json json;
    render_index json;
    List.iter render_reports_group report.all.groups
end

let main () =
  Lwt_io.(read_lines stdin)
  |> Lwt_stream.iter (fun file -> Queue.add file files) >>= fun () ->
  let start_time = Unix.gettimeofday () in
  workers !Options.workers >>= fun () ->
  let duration = Unix.gettimeofday () -. start_time in
  Report.render (generate_report ~duration ());
  Lwt_io.eprintf "Done!\n"

let () =
  Lwt_main.run (main ())
