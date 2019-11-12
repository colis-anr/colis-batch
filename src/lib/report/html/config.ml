open Colis_batch_ext
open Colis_batch_config

let pp fmt config =
  fpf fmt "<dl>
      <dt>Workers</dt><dd>%d</dd>
      <dt>CPU Timeout</dt><dd>%Fs</dd>
      <dt>Memory Limit</dt><dd>%s</dd>
    </dl>"
    config.workers
    config.cpu_timeout
    config.memory_limit;
