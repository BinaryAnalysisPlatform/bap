open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm

type project = {
  arch    : arch;
  argv    : string array;
  program : disasm;
  symbols : string table;
  memory  : mem;
  annots  : value memmap;
  bil_of_insns : (mem * insn) list -> bil;
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

let visitors = ref []
let register v = visitors := v :: !visitors
let register' v = register (fun p -> v p; p)
let registered () = List.rev !visitors
