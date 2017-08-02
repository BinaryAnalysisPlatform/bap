open Core_kernel.Std
open Regular.Std
open Bap.Std

let sexpable_of_string t_of_sexp name =
  try Some (t_of_sexp @@ Sexp.of_string name)
  with Sexp.Of_sexp_error _ -> None

type t = Riscv_types.reg [@@deriving bin_io, compare, sexp]

let create reg : t option =
  sexpable_of_string t_of_sexp (Reg.name reg)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let hash (reg : t) = Hashtbl.hash reg
    let module_name = Some "RISCV.Reg"
    let version = "1.0.0"
    let pp fmt reg =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t reg)
  end)
