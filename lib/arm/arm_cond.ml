open Core_kernel.Std
open Regular.Std
open Bap.Std

open Arm_types

type t = cond [@@deriving bin_io, compare, sexp]

let of_int_exn = function
  | 0 ->  `EQ
  | 1 ->  `NE
  | 2 ->  `CS
  | 3 ->  `CC
  | 4 ->  `MI
  | 5 ->  `PL
  | 6 ->  `VS
  | 7 ->  `VC
  | 8 ->  `HI
  | 9 ->  `LS
  | 10 -> `GE
  | 11 -> `LT
  | 12 -> `GT
  | 13 -> `LE
  | 14 -> `AL
  | n -> invalid_argf "not a condition: %d" n ()

let create w =
  let open Or_error in
  Word.to_int w >>= fun w ->
  try_with (fun () -> of_int_exn w)

include Regular.Make(struct
    type t = cond [@@deriving bin_io, compare, sexp]
    let hash (cond : t) = Hashtbl.hash cond
    let module_name = Some "Arm.Cond"
    let version = "0.1"
    let pp fmt cond =
      Format.fprintf fmt "%a" Sexp.pp (sexp_of_t cond)
  end)
