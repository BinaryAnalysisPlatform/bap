open Core_kernel.Std
open Regular.Std
open Bap.Std
open Arm_helpers

type t = Arm_types.reg [@@deriving bin_io, compare, sexp]

let create reg : t option =
  sexpable_of_string t_of_sexp (Reg.name reg)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let hash (reg : t) = Hashtbl.hash reg
    let module_name = Some "ARM.Reg"
    let version = "0.1"
    let pp fmt reg =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t reg)
  end)
