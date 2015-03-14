open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_basic
open Image_internal_std

module Rec = Bap_disasm_rec
module Insn = Bap_disasm_insn
type insn = Insn.t with compare, bin_io, sexp
type edge = Rec.edge with compare, sexp
type jump = Rec.jump with compare, sexp


let insns_of_decoded_list init : (mem * insn) seq =
  let open Seq.Step in
  Seq.unfold_step ~init ~f:(function
      | [] -> Done
      | (mem,_,_) as x :: xs -> match Insn.of_decoded x with
        | None -> Skip xs
        | Some insn -> Yield ((mem, insn), xs))


include Rec.Block

let uw = function
  | Some x -> x
  | _ -> failwith "Failed to unwind option"

let insns blk =
  Rec.Block.insns blk |> insns_of_decoded_list
let terminator blk = insns blk |> Seq.to_list_rev |> List.hd_exn |> snd
let leader blk = insns blk |> Seq.hd_exn |> snd
let of_rec_block = Fn.id

let () = Pretty_printer.register ("Bap_disasm_block.pp")

module Edge = struct
  type t = edge with compare
  let default = `Fall
end

module Graph =
  Graph.Persistent.Digraph.AbstractLabeled(struct
    type nonrec t = t option
  end)(Edge)

module Vis = Addr.Set

let unresolved = Graph.V.create None

let to_graph ?bound entry =
  let bounded addr =
    Option.value_map bound
      ~default:true ~f:(fun m -> Memory.contains m addr) in
  let skip visited blk =
    let addr = addr blk in
    Vis.mem visited addr || not (bounded addr) in
  let rec build gr vis src =
    if skip vis src then (gr,vis)
    else Seq.fold ~init:(gr,Vis.add vis (addr src)) (dests src)
        ~f:(fun (gr,vis) -> function
            | `Unresolved kind ->
              let src = Graph.V.create (Some src) in
              let kind = (kind :> edge) in
              let edge = Graph.E.create src kind unresolved in
              Graph.add_edge_e gr edge, vis
            | `Block (dst,kind) ->
              let v1 = Graph.V.create (Some src)
              and v2 = Graph.V.create (Some dst) in
              let edge = Graph.E.create v1 kind v2 in
              let gr = Graph.add_edge_e gr edge in
              build gr vis dst) in
  build Graph.empty Vis.empty entry |> fst
