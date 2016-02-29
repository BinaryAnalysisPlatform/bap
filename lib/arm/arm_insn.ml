open Core_kernel.Std
open Regular.Std
open Bap.Std
open Arm_helpers

type t = Arm_types.insn with bin_io, compare, sexp

let of_name name =
  sexpable_of_string t_of_sexp name

let of_basic insn = of_name (Disasm_expert.Basic.Insn.name insn)
let create insn = of_name (Insn.name insn)

include Regular.Make(struct
    type nonrec t = t with bin_io, compare, sexp
    let module_name = Some "Arm.Insn"
    let version = "0.1"
    let pp fmt insn =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t insn)
    let hash (insn : t) = Hashtbl.hash insn
  end)
