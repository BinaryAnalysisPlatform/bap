open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_basic

module Rec = Bap_disasm_rec
module Insn = Bap_disasm_insn
type insn = Insn.t with compare, bin_io, sexp


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
