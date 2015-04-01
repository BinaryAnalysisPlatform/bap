open Core_kernel.Std
open Bap.Std

let pr ch fms = Format.fprintf ch fms

let pp_word ch word =
  pr ch "Int(%s,%d)"
    (Word.string_of_value ~hex:false word)
    (Word.bitwidth word)

let pp_endian ch = function
  | BigEndian -> pr ch "BigEndian()"
  | LittleEndian -> pr ch "LittleEndian()"

let pp_size ch size =
  pr ch "%d" (Size.to_bits size)

let pp_sexp sexp ch x =
  pr ch "%a" Sexp.pp (sexp x)

let rec pp_list pp ch = function
  | [] -> ()
  | [x] -> pr ch "%a" pp x
  | x :: xs -> pr ch "%a, %a" pp x (pp_list pp) xs


module Asm = struct
  open Disasm
  let pp_pred ch kind =
    pr ch "%a()" (pp_sexp Basic.sexp_of_pred) kind
end

module Arm = struct
  open Disasm
  let pp_op ch = function
    | ARM.Op.Imm imm -> pr ch "Imm(%a)" pp_word imm
    | ARM.Op.Reg reg -> pr ch "Reg(%a())" ARM.Reg.pp reg

  let pp_insn ch (insn,ops) =
    pr ch "%a(%a)"
      (pp_sexp ARM.Insn.sexp_of_t) insn
      (pp_list pp_op) ops
end

let to_string pp x = Format.asprintf "%a" pp x
let to_strings pp lst = List.map ~f:(fun x -> Format.asprintf "%a" pp x) lst

let strings_of_bil = to_strings Stmt.pp_adt
let strings_of_ops = to_strings Op.pp_adt
let strings_of_preds = to_strings Asm.pp_pred
let string_of_arm insn ops = to_string Arm.pp_insn (insn,ops)
let string_of_endian = to_string pp_endian
let strings_of_kinds ks =
  strings_of_preds (ks :> Disasm_expert.Basic.pred list)



module Lex = struct
  open Re

  let spaces  = rep space
  let constr  = seq [upper; rep alpha]
  let open_p  = seq [spaces; char '('; spaces]
  let close_p = seq [spaces; char ')'; spaces]
  let integer = rep digit
  let quotes  = set "\""
  let no_args = seq [open_p; spaces; close_p]
  let comma   = seq [spaces; char ','; spaces]
  let size    = alt [str "32"; str "64"]
  let parens expr = seq [open_p; expr; close_p]

  let adt = compile @@ seq [
      group constr;
      parens @@ group @@ rep any;
    ]


  let nullary = compile @@ seq [spaces; group constr; spaces; no_args]

  let word = compile @@ seq [
      group constr;
      parens @@ seq [
        group integer;
        comma;
        group size;
      ]
    ]

end

module Parse = struct
  open Re
  open Or_error

  type 'a t = string -> 'a Or_error.t

  let parse re str = try get_all @@ exec re str with Not_found -> [| |]

  let sexp conv v =
    try Ok (conv @@ Sexp.of_string v)
    with exn -> errorf "Unknown constructor: %s" v

  let nullary conv str = match parse Lex.nullary str with
    | [| _; constr|] -> sexp conv constr
    | _ -> errorf "Malformed nullary constructor '%s'" str

  let int64 str =
    try Ok (Int64.of_string str)
    with exn -> errorf "'%s' is expected to be a number" str

  let word str = match parse Lex.word str with
    | [| _; "Int"; v; "32" |] -> int64 v >>| Word.of_int64 ~width:32
    | [| _; "Int"; v; "64" |] -> int64 v >>| Word.of_int64 ~width:64
    | _ -> errorf "'%s' doesn't match 'Int(num,size)'" str


  let kind = nullary Disasm_expert.Basic.kind_of_sexp
  let pred = nullary Disasm_expert.Basic.pred_of_sexp
  let endian = nullary endian_of_sexp
end
