open Core_kernel.Std
open Regular.Std
open Bap.Std

open Riscv_types

type t = cond [@@deriving bin_io, compare, sexp]

let of_int_exn = function
  | 0 -> `EQ
  | 1 -> `NE
  | 2 -> `GE
  | 3 -> `GEU
  | 4 -> `LT
  | 5 -> `LTU
  | 6 -> `AL
  | n -> invalid_argf "not a condition: %d" n ()

let create w =
  let open Or_error in
  Word.to_int w >>= fun w ->
  try_with (fun () -> of_int_exn w)

include Regular.Make(struct
    type t = cond [@@deriving bin_io, compare, sexp]
    let hash (cond : t) = Hashtbl.hash cond
    let module_name = Some "Riscv.Cond"
    let version = "1.0.0"
    let pp fmt cond =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t cond)
  end)
