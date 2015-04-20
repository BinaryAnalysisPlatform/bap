open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm

type t = {
  arch    : arch;
  disasm  : disasm;
  memory  : value memmap;
  storage : value String.Map.t;
  symbols : string table;
  base    : mem;
}

type color = [
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white
] with sexp

let text = Tag.register "text" sexp_of_string
let html = Tag.register "html" sexp_of_string
let comment = Tag.register "comment" sexp_of_string
let python = Tag.register "python" sexp_of_string
let shell = Tag.register "shell" sexp_of_string
let mark = Tag.register "mark" sexp_of_unit
let color = Tag.register "color" sexp_of_color
let weight = Tag.register "weight" sexp_of_float

let plugins : (string array -> t -> t) list ref = ref []
let register_plugin_with_args p =
  plugins := p :: !plugins
let register_plugin_with_args' v =
  register_plugin_with_args (fun a p -> v a p; p)
let register_plugin v = register_plugin_with_args (fun _arg p -> v p)
let register_plugin' v = register_plugin (fun p -> v p; p)
let plugins () = List.rev !plugins
