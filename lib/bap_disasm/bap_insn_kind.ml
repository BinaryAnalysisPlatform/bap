type branch = [
  | `conditional_branch
  | `unconditional_branch
  | `indirect_branch
] with compare, enumerate, sexp

type affecting_control = [
  | branch
  | `return
  | `call
  | `barrier
  | `terminator
  | `may_affect_control_flow
] with compare, enumerate, sexp

type having_side_effect = [
  | `may_load
  | `may_store
] with compare, enumerate, sexp

type t = [
  | affecting_control
  | having_side_effect
] with compare, enumerate, sexp
