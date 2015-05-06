open Bap.Std
open Core_kernel.Std
open Or_error

let with_byteweight bin : Addr.Set.t t =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = sprintf "bap-byteweight dump -i byteweight %S > %S" bin tmp in
  if Sys.command cmd = 0
  then return (In_channel.with_file tmp ~f:Symbols.read_addrset)
  else error_string cmd


let with_ida ~which_ida bin : Addr.Set.t t =
  Image.create bin >>= fun (img, _warns) ->
  let arch = Image.arch img in
  Ida.create ?ida:(Some which_ida) bin >>| fun ida ->
  Table.foldi (Image.sections img) ~init:Addr.Set.empty ~f:(fun mem sec ida_syms ->
      if Image.Sec.is_executable sec then
        let sym_tbl = Ida.(get_symbols ida arch mem) in
        Seq.fold Seq.(Table.regions sym_tbl >>| Memory.min_addr)
          ~init:ida_syms
          ~f:(fun s sym -> Addr.Set.add s sym)
      else ida_syms)
