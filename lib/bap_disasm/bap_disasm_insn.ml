open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_types

module Insn = Bap_disasm_basic.Insn

type t = {
  code : int;
  name : string;
  asm  : string;
  bil  : bil;
  ops  : Op.t array;
} with bin_io, fields, sexp

type op = Op.t with bin_io, compare, sexp

let compare t1 t2 =
  let r = Int.compare t1.code t2.code in
  if r = 0 then String.compare t1.asm t2.asm else r

let normalize_asm asm =
  String.substr_replace_all asm ~pattern:"\t"
    ~with_:" " |> String.strip

let of_basic ?bil insn = {
  code = Insn.code insn;
  name = Insn.name insn;
  asm  = normalize_asm (Insn.asm insn);
  bil  = Option.value bil ~default:[];
  ops  = Insn.ops insn;
}

let of_decoded = function
  | _, Some insn, bil -> Some (of_basic ?bil insn)
  | _, None,_ -> None

include Regular.Make(struct
    type nonrec t = t with sexp, bin_io, compare
    let hash = code
    let module_name = "Bap_disasm_insn"

    let string_of_ops ops =
      Array.map ops ~f:Op.to_string |> Array.to_list |>
      String.concat ~sep:","

    let pp fmt insn =
      let open Format in
      pp_print_string fmt insn.asm;
      pp_print_tbreak fmt 0 10;
      Format.fprintf fmt "; %s(%s)" insn.name (string_of_ops insn.ops)
  end)
