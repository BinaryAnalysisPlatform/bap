open Core_kernel
open Or_error
open Bap.Std

open Thumb_types
open Thumb_utils

module Basic     = Disasm_expert.Basic
module Env = Thumb_env

let move dst src = match dst with
    | `Reg reg -> [assn (Env.of_reg reg) (exp_of_op src)]
    | _ -> raise (Lifting_failed "dest is not reg")

let movei8 dst src = match dst with
    | `Reg reg -> let dest = Env.of_reg reg in
      Thumb_flags.set_nzf Bil.(var dest) reg32_t @ [assn dest (exp_of_op src)]
    | _ -> raise (Lifting_failed "dest is not reg")