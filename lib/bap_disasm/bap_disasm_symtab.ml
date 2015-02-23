open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm
open Or_error

module Block = Bap_disasm_block
module Insn = Bap_disasm_insn

type t = string table


let symbol_span roots base entry =
  let visited = Addr.Table.create () in
  let border blk = Block.memory blk |> Memory.max_addr in
  let rec loop maddr p =
    Seq.fold ~init:maddr (Block.dests p) ~f:(fun maddr -> function
        | `Unresolved _ -> maddr
        | `Block (blk,_)
          when Addr.Table.mem roots (Block.addr blk) -> maddr
        | `Block (_,`Jump)
          when Insn.is_call (Block.terminator p) -> maddr
        | `Block (blk,_) ->
          let visit =
            Addr.Table.add visited ~key:(Block.addr blk) ~data:() in
          match visit with
          | `Duplicate -> maddr
          | `Ok ->
            let baddr = Addr.(max maddr (border blk)) in
            loop baddr blk) in
  try_with (fun () -> loop (border entry) entry) >>= fun maddr ->
  Memory.range base (Block.addr entry) maddr  >>= fun sym ->
  return sym

let dest_of_bil bil =
  (object inherit [word] Bil.finder
    method! enter_int dst goto =
      if in_jmp then goto.return (Some dst);
      goto
  end)#find bil

let dest_of_insn insn =
  match Insn.bil insn with
  | _ :: _ as bil -> dest_of_bil bil
  | [] -> None

let create_roots_table roots cfg =
  let table = Addr.Table.create () in
  let name addr =
    sprintf "sub_%s" @@ Addr.string_of_value addr in
  List.iter roots ~f:(fun addr ->
      match Addr.Table.add table ~key:addr ~data:(name addr) with
      | `Duplicate | `Ok -> ());
  Table.iter cfg ~f:(fun blk ->
      let term = Block.terminator blk in
      if Insn.is_call term then match dest_of_insn term with
        | None -> ()
        | Some w ->
          let _ : string =
            Addr.Table.find_or_add table w ~default:(fun () -> name w) in
          ());
  table

let create roots base cfg =
  let roots = create_roots_table roots cfg in
  Table.fold cfg ~init:Table.empty ~f:(fun blk syms ->
      match Addr.Table.find roots (Block.addr blk) with
      | None -> syms
      | Some name ->
        let r =
          symbol_span roots base blk >>= fun mem ->
          Table.add syms mem name in
        match r with
        | Ok syms -> syms
        | Error err -> syms)
