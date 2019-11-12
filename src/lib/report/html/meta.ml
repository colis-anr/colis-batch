open Colis_batch_ext
open Colis_batch_report_common.Meta

let pp fmt meta =
  fpf fmt "<dl>
      <dt>Start time</dt><dd>%a</dd>
      <dt>End time</dt><dd>%a</dd>
      <dt>Duration</dt><dd>%.0fs</dd>
    </dl>"
    Unix.pp_time meta.start_time
    Unix.pp_time meta.end_time
    meta.duration
