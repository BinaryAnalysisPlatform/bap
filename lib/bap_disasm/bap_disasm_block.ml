open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm_basic

module Insn = Bap_disasm_insn
type insn = Insn.t with compare, bin_io, sexp
type jump = [
  | `Jump
  | `Cond
] with compare, sexp
type edge = [jump | `Fall] with compare,sexp

type t = {
  addr : addr;
  mem : mem;
  insns : (mem * insn) list;
  term : insn;
  lead : insn;
} with fields, sexp_of

let create mem insns =
  match insns with
  | [] -> invalid_arg "Block.create: empty insns"
  | _ ->
    let addr = Memory.min_addr mem in
    let lead = List.hd_exn insns |> snd in
    let term = List.last_exn insns |> snd in
    {addr; mem; insns; term; lead}

type repr = addr * int with compare
let compare x y =
  let repr x = x.addr,Memory.length x.mem in
  compare_repr (repr x) (repr y)

let memory = mem
let leader = lead
let terminator = term

include Opaque.Make(struct
    type nonrec t = t with compare
    let hash t = Addr.hash t.addr
  end)



include Printable(struct
    type nonrec t = t
    let pp ppf x =
      Format.fprintf ppf "[%a,%a]"
        Addr.pp (Memory.min_addr x.mem)
        Addr.pp (Memory.max_addr x.mem)

    let module_name = Some "Bap.Std.Block"
  end)
