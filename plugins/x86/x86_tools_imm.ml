open Core_kernel
open Bap.Std

type t = imm

let of_imm imm = imm

let get ~width t =
  Imm.to_word t ~width:(Size.in_bits width) |>
  fun x -> Bil.int (Option.value_exn x)
