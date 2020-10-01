open Core_kernel

type predicate =
  | Is_true
  | Is_invalid
  | Is_return
  | Is_call
  | Is_barrier
  | Is_terminator
  | Is_branch
  | Is_indirect_branch
  | Is_conditional_branch
  | Is_unconditional_branch
  | May_affect_control_flow
  | May_store
  | May_load
[@@deriving compare, sexp]

type op =
  | Reg
  | Imm
  | Fmm
  | Insn
[@@deriving compare, sexp]

module type S = sig
  type t
  val delete : t -> unit
  val set_memory : t -> int64 -> Bigstring.t -> off:int -> len:int -> unit
  val store_predicates : t -> bool -> unit
  val store_asm_string : t -> bool -> unit
  val insn_table : t -> Bigstring.t
  val reg_table : t -> Bigstring.t
  val predicates_clear : t -> unit
  val predicates_push : t -> predicate -> unit
  val is_supported : t -> predicate -> bool
  val set_offset : t -> int -> unit
  val offset : t -> int
  val run : t -> unit
  val insns_clear : t -> unit
  val insns_size : t -> int
  val insn_size : t -> insn:int -> int
  val insn_name : t -> insn:int -> int
  val insn_code : t -> insn:int -> int
  val insn_offset : t -> insn:int -> int
  val insn_asm_size : t -> insn:int -> int
  val insn_asm_copy : t -> insn:int -> Bytes.t -> unit
  val insn_satisfies : t -> insn:int -> predicate -> bool
  val insn_ops_size : t -> insn:int -> int
  val insn_op_type : t -> insn:int -> oper:int -> op
  val insn_op_reg_name : t -> insn:int -> oper:int -> int
  val insn_op_reg_code : t -> insn:int -> oper:int -> int
  val insn_op_imm_value : t -> insn:int -> oper:int -> int64
  val insn_op_imm_small_value : t -> insn:int -> oper:int -> int
  val insn_op_fmm_value : t -> insn:int -> oper:int -> float
end
