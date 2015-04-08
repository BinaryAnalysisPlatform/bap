open Bap.Std
open Core_kernel.Std
open Or_error

exception Bad_user_input

let byteweight bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  Printf.fprintf stderr "%s\n" tmp;
  let cmd = Printf.sprintf "bap-byteweight find -x %s > %s" bin tmp in
  Printf.fprintf stderr "%s\n" cmd;
  let _ = Unix.system cmd in
  Symbols.read_addrset tmp

let usersource = Symbols.read_addrset
  (* let s = Addr.Hash_set.of_list [Addr.of_string "0x1234:32"] in
  Symbols.write_addrset ~filename:f s;
  Symbols.read_addrset f *)

let symbols bin =
  let tmp = Filename.temp_file "bw_" ".output" in
  Printf.fprintf stderr "%s\n" tmp;
  let cmd = Printf.sprintf "bap-byteweight symbols -x %s > %s" bin tmp in
  Printf.fprintf stderr "%s\n" cmd;
  let _ = Unix.system cmd in
  try
    Symbols.read_addrset tmp
  with _ -> raise Bad_user_input

let ida bin : Addr.Hash_set.t =
  let roots_of_table t : addr list =
    Seq.(Table.regions t >>| Memory.min_addr |> to_list) in
  let res =
    Printf.printf "%s\n%!" bin;
    Image.create bin >>= fun (img, _warns) ->
      let arch = Image.arch img in
    Ida.create ~ida:"idaq64" bin >>| fun ida ->
      Table.foldi (Image.sections img) ~init:[] ~f:(fun mem sec ida_syms ->
        if Section.is_executable sec then
          let ida_syms_t = roots_of_table Ida.(get_symbols ida arch mem) in
          Printf.printf "%d\n" @@ List.length ida_syms_t;
          ida_syms @ ida_syms_t
        else ida_syms) in
  match res with
  | Ok l -> Printf.printf "IDA items: %d\n" @@ List.length l; Addr.Hash_set.of_list l
  | Error err -> Addr.Hash_set.of_list []
