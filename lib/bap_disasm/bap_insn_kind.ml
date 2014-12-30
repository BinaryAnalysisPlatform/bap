type branch = [
  | `Conditional_branch
  | `Unconditional_branch
  | `Indirect_branch
] with bin_io, compare, enumerate, sexp

type affecting_control = [
  | branch
  | `Return
  | `Call
  | `Barrier
  | `Terminator
  | `May_affect_control_flow
] with bin_io, compare, enumerate, sexp

type having_side_effect = [
  | `May_load
  | `May_store
] with bin_io, compare, enumerate, sexp

type t = [
  | affecting_control
  | having_side_effect
] with bin_io, compare, enumerate, sexp
