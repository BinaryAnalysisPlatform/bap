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

module Var = struct
  let pp_ty ch = function
    | Type.Imm n -> pr ch "Imm(%d)" n
    | Type.Mem (n,m) -> pr ch "Mem(%a,%a)" pp_size n pp_size m

  let pp_var ch v =
    pr ch "Var(\"%s\",%a)" Var.(name v) pp_ty Var.(typ v)

end

module Exp = struct
  open Exp
  open Var

  let rec pp ch = function
    | Load (x,y,e,s) ->
      pr ch "Load(%a,%a,%a,%a)" pp x pp y pp_endian e pp_size s
    | Store (x,y,z,e,s) ->
      pr ch "Store(%a,%a,%a,%a,%a)" pp x pp y pp z pp_endian e pp_size s
    | BinOp (op,x,y) ->
      pr ch "%a(%a,%a)" (pp_sexp sexp_of_binop) op pp x pp y
    | UnOp (op,x) ->
      pr ch "%a(%a)" (pp_sexp sexp_of_unop) op pp x
    | Var v -> pp_var ch v
    | Int w -> pp_word ch w
    | Cast (ct,sz,ex) ->
      pr ch "%a(%d,%a)" (pp_sexp sexp_of_cast) ct sz pp ex
    | Let (v,e1,e2) -> pr ch "Let(%a,%a,%a)" pp_var v pp e1 pp e2
    | Unknown (s,t) -> pr ch "Unknown(\"%s\",%a)" s pp_ty t
    | Ite (e1,e2,e3) -> pr ch "Ite(%a,%a,%a)" pp e1 pp e2 pp e3
    | Extract (n,m,e) -> pr ch "Extract(%d,%d,%a)" n m pp e
    | Concat (e1,e2) -> pr ch "Concat(%a,%a)" pp e1 pp e2
end

module Stmt = struct
  open Stmt
  open Var
  let rec pp ch = function
    | Move (v,e) -> pr ch "Move(%a,%a)" pp_var v Exp.pp e
    | Jmp e -> pr ch "Jmp(%a)" Exp.pp e
    | Special s -> pr ch "Special(\"%s\")" s
    | While (e,ss) -> pr ch "While(%a, (%a))" Exp.pp e pps ss
    | If (e,xs,ys) -> pr ch "If(%a, (%a), (%a))" Exp.pp e pps xs pps ys
    | CpuExn n -> pr ch "CpuExn(%d)" n
  and pps ch = function
    | []  -> ()
    | [s] -> pp ch s
    | s :: ss -> pr ch "%a, %a" pp s pps ss
end


module Asm = struct
  open Disasm
  let pp_pred ch kind =
    pr ch "%a()" (pp_sexp Basic.sexp_of_pred) kind

  let pp_op ch = function
    | Op.Imm imm -> pr ch "Imm(0x%Lx)" (Imm.to_int64 imm)
    | Op.Fmm fmm -> pr ch "Fmm(%g)" (Fmm.to_float fmm)
    | Op.Reg reg -> pr ch "Reg(\"%a\")" Reg.pp reg
end

module Arm = struct
  open Disasm
  let pp_op ch = function
    | ARM.Op.Imm imm -> pr ch "Imm(%a)" pp_word imm
    | ARM.Op.Reg reg -> pr ch "Reg(%a())" ARM.Reg.pp reg

  let rec pp_ops ch = function
    | [] -> ()
    | [x] -> pr ch "%a" pp_op x
    | x :: xs -> pr ch "%a, %a" pp_op x pp_ops xs

  let pp_insn ch (insn,ops) =
    pr ch "%a(%a)" (pp_sexp ARM.Insn.sexp_of_t) insn pp_ops ops
end

let to_string pp x = Format.asprintf "%a" pp x
let to_strings pp lst = List.map ~f:(fun x -> Format.asprintf "%a" pp x) lst

let strings_of_bil = to_strings Stmt.pp
let strings_of_ops = to_strings Asm.pp_op
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
