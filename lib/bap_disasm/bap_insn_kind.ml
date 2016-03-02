type branch = [
  | `Conditional_branch
  | `Unconditional_branch
  | `Indirect_branch
] [@@deriving bin_io, compare, enumerate, sexp]

type affecting_control = [
  | branch
  | `Return
  | `Call
  | `Barrier
  | `Terminator
  | `May_affect_control_flow
] [@@deriving bin_io, compare, enumerate, sexp]

type having_side_effect = [
  | `May_load
  | `May_store
] [@@deriving bin_io, compare, enumerate, sexp]

type t = [
  | affecting_control
  | having_side_effect
] [@@deriving bin_io, compare, enumerate, sexp]
