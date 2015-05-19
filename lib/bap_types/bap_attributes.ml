open Core_kernel.Std

open Bap_value.Tag

module Color = struct
  type t = [
    | `black
    | `red
    | `green
    | `yellow
    | `blue
    | `magenta
    | `cyan
    | `white
  ] with bin_io, compare, sexp
  let pp ppf color =
    Format.fprintf ppf "%a" Sexp.pp (sexp_of_t color)
end

type color = Color.t with bin_io, compare, sexp

let comment = register (module String)
    ~name:"comment"
    ~uuid:"4b974ab3-bf3b-4a83-8c62-299bca70f02a"

let python = register (module String)
    ~name:"python"
    ~uuid:"831a6268-0ca8-4c1b-b4d6-076995f49d84"

let shell = register (module String)
    ~name:"shell"
    ~uuid:"8c2c459d-6f3e-42f7-bce7-bf5cfa280f24"

let mark = register (module Unit)
    ~name:"mark"
    ~uuid:"8e9801dc-0c64-4943-acf4-bfd02347af91"

let color = register (module Color)
    ~name:"color"
    ~uuid:"1938c44a-149d-4c71-832a-7f484be800cc"

let weight = register (module Float)
    ~name:"weight"
    ~uuid:"657366ea-9a28-4e5e-8341-c545d861732b"

let target_addr = register (module Bap_bitvector)
    ~name:"target_addr"
    ~uuid:"7bcef7c0-0b37-4167-887a-eba0d68891fe"

let target_name = register (module String)
    ~name:"target_name"
    ~uuid:"35d9334f-7c17-46f7-8ff9-9430aa1293ac"

let subroutine_addr = register (module Bap_bitvector)
    ~name:"subroutine_addr"
    ~uuid:"76bfd31c-05fb-48af-bfc1-721720710f0f"

let subroutine_name = register (module String)
    ~name:"subroutine_name"
    ~uuid:"86fe023d-2637-4d92-baac-a420f518f250"

let filename = register (module String)
    ~name:"filename"
    ~uuid:"9701d189-24e3-4348-8610-0dedf780d06b"
