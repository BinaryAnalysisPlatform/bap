open Core_kernel
open Regular.Std
open Bap.Std
open Arm_helpers

type t = Arm_types.insn [@@deriving bin_io, compare, sexp]

let is_thumb2 = String.is_prefix ~prefix:"t2"
let recode_as_arm = String.chop_prefix_exn ~prefix:"t2"

let of_name name =
  let name = if is_thumb2 name then recode_as_arm name else name in
  sexpable_of_string t_of_sexp name

let of_basic insn = of_name (Disasm_expert.Basic.Insn.name insn)
let create insn = of_name (Insn.name insn)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let module_name = Some "Arm.Insn"
    let version = "1.0.0"
    let pp fmt insn =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t insn)
    let hash (insn : t) = Hashtbl.hash insn
  end)
