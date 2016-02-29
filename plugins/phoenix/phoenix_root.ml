open Core_kernel.Std

exception Target_directory_is_a_file

type t = string * string
type channel = Format.formatter


let (/) = Filename.concat

let mkdir dir =
  if not (Sys.file_exists dir)
  then Unix.mkdir dir 0o777
  else if not (Sys.is_directory dir)
  then raise Target_directory_is_a_file

let touch_bases names =
  List.iter names ~f:(fun p -> mkdir (Filename.dirname p))

let create ?root fname =
  let base = Option.value root ~default:(Sys.getcwd ()) in
  mkdir base;
  let base =
    if base <> Sys.getcwd () then base
    else base / Filename.basename fname in
  mkdir base;
  base,Filename.basename fname

let with_file name ~f =
  Out_channel.with_file name ~f:(fun chan ->
      let fmt = Format.formatter_of_out_channel chan in
      Text_tags.install fmt "html";
      f fmt)

let with_files_in_folders folders (base,_) name ~f =
  let names = List.map folders ~f:(fun (p,ext) -> base/p/name^"."^ext) in
  touch_bases names;
  List.iter names ~f:(with_file ~f)

let with_cfg_file =
  with_files_in_folders  ["cfg", "cfg"; "sacfg", "sacfg"]
let with_bil_file =
  with_files_in_folders  ["bil", "html"; "hil", "html"]

let with_index_file (base,name) ~f = with_file (base/name^".json") ~f

let with_funcs_file (base,name) ~f = with_file (base/name^".functionlist") ~f

let with_dump_file (base,name) ~f = with_file (base/name^".html") ~f

let path = fst
