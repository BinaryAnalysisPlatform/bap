open Bap.Std
open Core_kernel.Std
open Or_error

let with_byteweight bin : addr seq Or_error.t =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = sprintf "bap-byteweight dump -i byteweight %S > %S" bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:Symbols.read_addrs)
  else error_string cmd


let with_ida ~which_ida bin : addr seq Or_error.t =
  Image.create bin >>= fun (img, _warns) ->
  let arch = Image.arch img in
  Ida.create ?ida:(Some which_ida) bin >>| fun ida ->
  let syms = Ida.get_symbols ida arch in
  Seq.of_list @@ List.map syms ~f:(fun (_, es, _) -> es)
