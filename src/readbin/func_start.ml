open Bap.Std
open Core_kernel.Std
open Or_error

exception Bad_user_input of string

let bap_byteweight bin tool =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = Printf.sprintf "bap-byteweight dump -i %s %S > %S" tool bin tmp in
  try
    let _ = Unix.system cmd in
    Symbols.read_addrset tmp
  with _ -> raise (Bad_user_input tool)

let byteweight bin = bap_byteweight bin "BW"

let symbols bin = bap_byteweight bin "SymTbl"

let user = Symbols.read_addrset

let ida ?use_ida bin : Addr.Set.t =
  let res =
    Image.create bin >>= fun (img, _warns) ->
    let arch = Image.arch img in
    Ida.create ?ida:use_ida bin >>| fun ida ->
    Table.foldi (Image.sections img) ~init:Addr.Set.empty ~f:(fun mem sec ida_syms ->
        if Section.is_executable sec then
          let sym_tbl = Ida.(get_symbols ida arch mem) in
          Seq.fold Seq.(Table.regions sym_tbl >>| Memory.min_addr)
            ~init:ida_syms
            ~f:(fun s sym -> Addr.Set.add s sym)
        else ida_syms) in
  match res with
  | Ok l -> l
  | Error err -> Printf.printf "IDA Error: %s\n" @@ Error.to_string_hum err;
    Addr.Set.empty
