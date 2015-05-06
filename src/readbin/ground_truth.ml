open Bap.Std
open Core_kernel.Std
open Or_error


let from_unstripped_bin bin : Addr.Set.t t =
  let tmp = Filename.temp_file "bw_" ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i %s %S > %S" "symbols"
      bin tmp in
  if Sys.command cmd = 0
  then return (In_channel.with_file tmp ~f:Symbols.read_addrset)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd


let from_symbol_file filename ~testbin : Addr.Set.t t =
  Image.create testbin >>| (fun (img, _errors) ->
      let arch = Image.arch img in
      Table.foldi (Image.sections img) ~init:Addr.Set.empty
        ~f:(fun mem sec t_fs ->
            if Image.Sec.is_executable sec then
              let sym_tbl = In_channel.with_file filename
                  ~f:(fun ic -> Symbols.read ic arch mem) in
              Seq.fold ~init:t_fs (Table.regions sym_tbl)
                ~f:(fun accum mem -> Addr.Set.add accum @@ Memory.min_addr mem)
            else t_fs))

