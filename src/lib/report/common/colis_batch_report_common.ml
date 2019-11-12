open Colis_batch_ext
module Package = Package
module Batch = Batch

let with_formatter_to_file ~prefix path f =
  let path = Filename.concat_l (prefix :: path) in
  Filesystem.mkdir ~parents:true (Filename.dirname path);
  let ochan = open_out path in
  let fmt = Format.formatter_of_out_channel ochan in
  let y = f fmt in
  Format.pp_print_flush fmt ();
  close_out ochan;
  y
