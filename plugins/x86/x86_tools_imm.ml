open Core_kernel.Std
open Bap.Std

type t = imm

let of_imm imm = imm

let get ~width t =
  Imm.to_word t ~width:(Size.in_bits width) |>
  Option.value_exn |>
  Bil.int

