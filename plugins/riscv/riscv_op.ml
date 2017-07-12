open Core_kernel.Std
open Regular.Std
open Bap.Std

module Riscv = Riscv_types

type t = Riscv.op [@@deriving bin_io, compare, sexp]

let create : op -> Riscv.op option =
  let open Option.Monad_infix in
  function
  | Op.Fmm fmm -> None
  | Op.Reg reg -> Riscv_reg.create reg >>| fun reg -> `Reg reg
  | Op.Imm imm ->
    Imm.to_word ~width:32 imm >>| fun imm -> `Imm imm

include Regular.Make(struct
    type t = Riscv.op [@@deriving bin_io, compare, sexp]
    let module_name = Some "Riscv.Op"
    let version = "1.0.0"
    let pp fmt op =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t op)
    let hash op = Hashtbl.hash op
  end)
