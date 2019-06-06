open Core_kernel

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

let comment = register (module String) ~name:"comment" ~uuid:"bap.std.attrs"
let python = register (module String) ~name:"python" ~uuid:"bap.std.attrs"
let shell = register (module String) ~name:"shell" ~uuid:"bap.std.attrs"
let mark = register (module Unit) ~name:"mark" ~uuid:"bap.std.attrs"
let color = register (module Color) ~name:"color" ~uuid:"bap.std.attrs"
let weight = register (module Float) ~name:"weight" ~uuid:"bap.std.attrs"
let address = register (module Bap_bitvector) ~name:"address" ~uuid:"bap.std.attrs"
let filename = register (module String) ~name:"filename" ~uuid:"bap.std.attrs"
let foreground = register (module Foreground) ~name:"foreground" ~uuid:"bap.std.attrs"
let background = register (module Background) ~name:"background" ~uuid:"bap.std.attrs"
