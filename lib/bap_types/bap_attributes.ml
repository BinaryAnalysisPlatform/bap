open Core_kernel
open Bap_core_theory

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
    | `gray
  ] [@@deriving bin_io, compare, sexp]
  let pp ppf color =
    Format.fprintf ppf "%a" Sexp.pp (sexp_of_t color)
end

module Foreground = struct
  type t = Color.t [@@deriving bin_io, compare, sexp]
  let to_ascii = function
    | `black   -> "\x1b[30m"
    | `red     -> "\x1b[31m"
    | `green   -> "\x1b[32m"
    | `yellow  -> "\x1b[33m"
    | `blue    -> "\x1b[34m"
    | `magenta -> "\x1b[35m"
    | `cyan    -> "\x1b[36m"
    | `white   -> "\x1b[37m"
    | `gray    -> "\x1b[1;30m"
  let pp ppf c = Format.fprintf ppf "%s" @@ to_ascii c
end

module Background = struct
  type t = Color.t [@@deriving bin_io, compare, sexp]
  let to_ascii : t -> string = function
    | `black   -> "\x1b[40m"
    | `red     -> "\x1b[41m"
    | `green   -> "\x1b[42m"
    | `yellow  -> "\x1b[43m"
    | `blue    -> "\x1b[44m"
    | `magenta -> "\x1b[45m"
    | `cyan    -> "\x1b[46m"
    | `white   -> "\x1b[47m"
    | `gray    -> "\x1b[1;40m"

  let pp ppf c = Format.fprintf ppf "%s" @@ to_ascii c
end

type color = Color.t [@@deriving bin_io, compare, sexp_poly]

let package = "bap"

let comment = register (module String)
    ~package
    ~public:true
    ~desc:"a human-readable comment"
    ~name:"comment"
    ~uuid:"4b974ab3-bf3b-4a83-8c62-299bca70f02a"

let python = register (module String)
    ~package
    ~name:"python"
    ~uuid:"831a6268-0ca8-4c1b-b4d6-076995f49d84"

let shell = register (module String)
    ~package
    ~name:"shell"
    ~uuid:"8c2c459d-6f3e-42f7-bce7-bf5cfa280f24"

let mark = register (module Unit)
    ~package
    ~public:true
    ~desc:"designates the program."
    ~name:"mark"
    ~uuid:"8e9801dc-0c64-4943-acf4-bfd02347af91"

let color = register (module Color)
    ~package
    ~public:true
    ~name:"color"
    ~desc:"colors the program."
    ~uuid:"1938c44a-149d-4c71-832a-7f484be800cc"

let weight = register (module Float)
    ~package
    ~public:true
    ~name:"weight"
    ~desc:"weighs the program."
    ~uuid:"657366ea-9a28-4e5e-8341-c545d861732b"

let address = register (module Bap_bitvector)
    ~package
    ~public:false
    ~name:"address"
    ~uuid:"7bcef7c0-0b37-4167-887a-eba0d68891fe"

let filename = register (module String)
    ~package
    ~public:false
    ~name:"filename"
    ~uuid:"9701d189-24e3-4348-8610-0dedf780d06b"

let foreground = register (module Foreground)
    ~package
    ~name:"foreground"
    ~uuid:"56b29739-2df4-4e6c-9f63-15e20edf1857"

let background = register (module Background)
    ~package
    ~name:"background"
    ~uuid:"9a80a9cc-4106-48fc-abf3-55d7b333e734"
