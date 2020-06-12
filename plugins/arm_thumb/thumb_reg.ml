open Core_kernel
open Regular.Std
open Bap.Std
open Thumb_helpers

type t = Thumb_types.reg [@@deriving bin_io, compare, sexp]

let create reg : t option =
  sexpable_of_string t_of_sexp (Reg.name reg)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let hash (reg : t) = Hashtbl.hash reg
    let module_name = Some "Thumb.Reg"
    let version = "1.0.0"
    let pp fmt reg =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t reg)
  end)
