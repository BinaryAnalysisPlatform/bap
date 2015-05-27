open Bap.Std
open Core_kernel.Std
open Or_error


let from_unstripped_bin bin : addr seq Or_error.t =
  let tmp = Filename.temp_file "bw_" ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i %s %S > %S" "symbols"
      bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:Symbols.read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd

let from_symbol_file filename ~testbin : addr seq Or_error.t =
  Image.create testbin >>| (fun (img, _errors) ->
      let arch = Image.arch img in
      let syms = In_channel.with_file filename ~f:(Symbols.read arch)
      in
      Seq.of_list @@ List.map syms ~f:(fun (_, es, _) -> es))
