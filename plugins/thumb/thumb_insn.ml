open Core_kernel
open Bap.Std
open Or_error

type t = Thumb_defs.insn [@@deriving bin_io, compare, sexp]
type regs = Thumb_defs.reg [@@deriving bin_io, compare, sexp]

let sexpable_of_string t_of_sexp name =
  try Some (t_of_sexp @@ Sexp.of_string name)
  with Sexp.Of_sexp_error _ -> None

let of_name name = 
  sexpable_of_string t_of_sexp name

let of_basic insn = of_name (Disasm_expert.Basic.Insn.name insn)
let create insn = of_name (Insn.name insn)

let create_reg reg : Thumb_defs.reg option =
  sexpable_of_string regs_of_sexp (Reg.name reg)

let create_op : op -> Thumb_defs.op option =
  let open Option.Monad_infix in
  function
  | Op.Fmm fmm -> None (* this should be later extended *)
  | Op.Reg reg -> create_reg reg >>| fun reg -> `Reg reg
  | Op.Imm imm -> Imm.to_word ~width:32 imm >>| fun imm -> `Imm imm

let arm_ops_exn ops () =
  Array.map (ops) ~f:(fun op ->
      Option.value_exn
        ~here:[%here]
        ~error:(Error.create "unsupported operand" op Op.sexp_of_t )
        (create_op op))

let arm_ops ops = try_with ~backtrace:true (arm_ops_exn ops)
