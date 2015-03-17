open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm
open Or_error

module Block = Bap_disasm_block
module Insn = Bap_disasm_insn

type t = string table


let symbol_span map visited roots base entry =
  let border blk = Block.memory blk |> Memory.max_addr in
  let overran =
    match Addr.Map.next_key map (Block.addr entry) with
    | Some (next,_) -> (fun addr -> addr >= next)
    | None -> (fun _ -> false) in
  let rec loop maddr p =
    Seq.fold ~init:maddr (Block.dests p) ~f:(fun maddr -> function
        | `Unresolved _ -> maddr
        | `Block (blk,_) when overran (Block.addr blk) -> maddr
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
  let map = Addr.Map.of_alist_exn (Addr.Table.to_alist roots) in
  let visited = Addr.Table.create () in
  Table.fold cfg ~init:Table.empty ~f:(fun blk syms ->
      match Addr.Table.find roots (Block.addr blk) with
      | None -> syms
      | Some name ->
        let r =
          symbol_span map visited roots base blk >>= fun mem ->
          Table.add syms mem name in
        match r with
        | Ok syms -> syms
        | Error err -> syms)


module Graph =
  Graph.Persistent.Digraph.AbstractLabeled(struct
    type t = mem * string
  end)(struct
    type t = addr with compare
    let default = Addr.b0
  end)

let to_graph blocks symbols =
  Table.foldi symbols ~init:Graph.empty ~f:(fun smem src gr ->
      Table.intersections blocks smem |>
      Seq.fold ~init:gr ~f:(fun gr (_,blk) ->
          Seq.fold ~init:gr (Block.succs blk) ~f:(fun gr suc ->
              match Table.find_addr symbols (Block.addr suc) with
              | Some (dmem,dst) ->
                if Addr.(Block.addr suc = Memory.min_addr dmem)
                then
                  let v1 = Graph.V.create (smem,src) in
                  let v2 = Graph.V.create (dmem,dst) in
                  let edge = Graph.E.create v1 (Block.addr blk) v2 in
                  Graph.add_edge_e gr edge
                else gr
              | _ -> gr)))
