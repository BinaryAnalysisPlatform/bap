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

  let uw = function
    | Some x -> x
    | _ -> failwith "Failed to unwind option"

  let insns blk =
    Rec.Block.insns blk |> insns_of_decoded_list
  let terminator blk = insns blk |> List.last_exn |> snd
  let leader blk = insns blk |> List.hd_exn |> snd
  let of_rec_block = Fn.id
end
include Block

let () = Pretty_printer.register ("Bap_disasm_block.pp")

module Edge = struct
  type t = edge with compare
  let default = `Fall
end


module Cfg = struct
  module Block = Block
  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (Block)(Edge)

  module Imperative =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
      (Block)(Edge)
end

module Vis = Addr.Set

module Build(G : Graph.Builder.S with type G.E.t = t * edge * t
                                  and type G.V.t = t) = struct
  let to_graph ?bound entry =
    let bounded addr =
      Option.value_map bound
        ~default:true ~f:(fun m -> Memory.contains m addr) in
    let skip visited blk =
      let addr = addr blk in
      Vis.mem visited addr || not (bounded addr) in
    let rec build gr vis src =
      if skip vis src then (gr,vis)
      else Seq.fold (dests src)
          ~init:(G.add_vertex gr src,Vis.add vis (addr src))
          ~f:(fun (gr,vis) -> function
              | `Unresolved kind -> (gr,vis)
              | `Block (dst,kind) ->
                let edge = Cfg.E.create src kind dst in
                let gr = G.add_edge_e gr edge in
                build gr vis dst) in
    entry, build (G.empty ()) Vis.empty entry |> fst
end
module Persistant = Build(Graph.Builder.P(Cfg))
module Imperative = Build(Graph.Builder.I(Cfg.Imperative))

let to_graph = Persistant.to_graph
let to_imperative_graph = Imperative.to_graph
