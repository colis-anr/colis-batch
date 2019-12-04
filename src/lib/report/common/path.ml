open Colis_batch_ext
module Model = Colis_batch_model

let root ~prefix = prefix

let dir path dir =
  Filename.(concat dir (concat_l path))

let file ?ext name dir =
  Filename.concat dir (name ^ match ext with None -> "" | Some ext -> "." ^ ext)

let json_file dir name =
  dir |> file name ~ext:"json"

let bin_file dir name =
  dir |> file name ~ext:"bin"

let main_json_file ~prefix =
  root ~prefix |> json_file "main"

let main_bin_file ~prefix =
  root ~prefix |> bin_file "main"

let package_dir ~prefix ~package =
  root ~prefix |> dir ["package"; package]

let package_json_file ~prefix ~package =
  package_dir ~prefix ~package |> json_file "main"

let package_bin_file ~prefix ~package =
  package_dir ~prefix ~package |> bin_file "main"

let package_summary_json_file ~prefix ~package =
  package_dir ~prefix ~package |> json_file "summary"

let package_summary_bin_file ~prefix ~package =
  package_dir ~prefix ~package |> bin_file "summary"

let get_lock_on filename =
  let oc = open_out_gen [Open_creat; Open_excl] 0o777 (filename ^ ".lock") in
  close_out oc

let free_lock_on filename =
  Sys.remove (filename ^ ".lock")

let with_lock_on filename f =
  get_lock_on filename;
  let v = try Ok (f ()) with exn -> Error exn in
  free_lock_on filename;
  match v with Ok v -> v | Error exn -> raise exn
