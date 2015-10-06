open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_basic
open Image_internal_std
open Bap_disasm_block_intf

module Rec = Bap_disasm_rec
module Insn = Bap_disasm_insn
type insn = Insn.t with compare, bin_io, sexp

let insns_of_decoded_list ds : (mem * insn) list =
  List.filter_map ds ~f:(function
      | mem, (Some insn, bil) -> Some (mem, Insn.of_basic ?bil insn)
      | _ -> None)

module Block = struct
  include Rec.Block
  let insns blk =
    Rec.Block.insns blk |> insns_of_decoded_list
  let terminator blk = insns blk |> List.last_exn |> snd
  let leader blk = insns blk |> List.hd_exn |> snd
  let of_rec_block = Fn.id
end


module Graph = Bap_graph_regular.Make(Block)(struct type t = edge end)

module Vis = Addr.Hash_set

let bounded bound addr =
  Option.value_map bound ~default:true ~f:(fun f -> f addr)

let skip bound visited blk =
  let addr = Block.addr blk in
  Hash_set.mem visited addr || not (bounded bound addr)

let to_graph ?bound entry =
  let vis = Vis.create () in
  let rec build gr (src : Block.t) =
    if skip bound vis src then gr
    else Seq.fold (Block.dests src)
        ~init:(Graph.Node.insert src gr)
        ~f:(fun gr -> function
            | `Unresolved _ -> gr
            | `Block (dst,(`Cond|`Jump))
              when not (bounded bound (Block.addr dst))
                   || Insn.is_call (Block.terminator src) -> gr
            | `Block (dst,kind) ->
              Hash_set.add vis (Block.addr src);
              let edge = Graph.Edge.create src dst kind in
              let gr = Graph.Edge.insert edge gr in
              build gr dst) in
  build Graph.empty entry

include Block
let () = Pretty_printer.register "Bap.Std.Block.pp"
