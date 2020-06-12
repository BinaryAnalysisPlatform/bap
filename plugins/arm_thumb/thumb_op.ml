open Core_kernel
open Regular.Std
open Bap.Std
open Or_error

module Arm = Thumb_types

type t = Arm.op [@@deriving bin_io, compare, sexp]


let create : op -> Arm.op option =
  let open Option.Monad_infix in
  function
  | Op.Fmm fmm -> None
  | Op.Reg reg -> Thumb_reg.create reg >>| fun reg -> `Reg reg
  | Op.Imm imm ->
    Imm.to_word ~width:32 imm >>| fun imm -> `Imm imm

include Regular.Make(struct
    type t = Arm.op [@@deriving bin_io, compare, sexp]
    let module_name = Some "Thumb.Op"
    let version = "1.0.0"
    let pp fmt op =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t op)
    let hash op = Hashtbl.hash op
  end)


let arm_ops_exn ops () =
  Array.map (ops) ~f:(fun op ->
      Option.value_exn
        ~here:[%here]
        ~error:(Error.create "unsupported operand" op Op.sexp_of_t )
        (create op))

let arm_ops ops = try_with ~backtrace:true (arm_ops_exn ops)