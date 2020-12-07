open Core_kernel
open Bap_core_theory
open Theory.Parser

include Bap_main.Loggers()

module Exp = Bap_exp.Exp
module Bil = Bap_bil
module Word = Bap_bitvector
module Type = Bap_common.Type
module Size = Bap_size
module Stmt = Bap_stmt.Stmt

type stmt = Bil.stmt


type context = [`Bitv | `Bool | `Mem ] [@@deriving sexp]
let fail exp ctx =
  error "ill-formed expression in %a ctxt: %a"
    Sexp.pp (sexp_of_context ctx) Bap_exp.pp exp

type exp = Bil.exp
module Var = Bap_var
let rec uncat acc : exp -> exp list = function
  | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
  | Concat (x,y) -> x::y::acc
  | x -> x::acc

let bits_of_var v = match Var.typ v with
  | Imm x -> x
  | _ -> failwith "not a bitv var"

let byte x = Exp.int (Word.of_int ~width:8 x)
let is_big e =
  Exp.int @@
  if [%compare.equal : Word.endian] e BigEndian
  then Word.b1 else Word.b0

let is_reg v = match Var.typ v with
  | Type.Imm 1 | Type.Mem _ -> false
  | _ -> true

let is_bit v = match Var.typ v with
  | Type.Imm 1 -> true
  | _ -> false

let is_mem v = match Var.typ v with
  | Type.Mem _ -> true
  | _ -> false

let bitv : type t r. (t,exp,r) bitv_parser =
  fun (module S) -> function
    | Cast (HIGH,n,x) -> S.high n x
    | Cast (LOW,n,x) -> S.low n x
    | Cast (UNSIGNED,n,x) -> S.unsigned n x
    | Cast (SIGNED,n,x) -> S.signed n x
    | BinOp(PLUS,x,y) -> S.add x y
    | BinOp(MINUS,x,y) -> S.sub x y
    | BinOp(TIMES,x,y) -> S.mul x y
    | BinOp(DIVIDE,x,y) -> S.div x y
    | BinOp(SDIVIDE,x,y) -> S.sdiv x y
    | BinOp(MOD,x,y) -> S.modulo x y
    | BinOp(SMOD,x,y) -> S.smodulo x y
    | BinOp(LSHIFT,x,y) -> S.lshift x y
    | BinOp(RSHIFT,x,y) -> S.rshift x y
    | BinOp(ARSHIFT,x,y) -> S.arshift x y
    | BinOp(AND,x,y) -> S.logand x y
    | BinOp(OR,x,y) -> S.logor x y
    | BinOp(XOR,x,y) -> S.logxor x y
    | UnOp(NEG,x) -> S.neg x
    | UnOp(NOT,x) -> S.not x
    | Load(m,k,e,s) ->
      S.load_word (Size.in_bits s) (is_big e) m k
    | Var v -> S.var (Var.name v) (bits_of_var v)
    | Int x -> S.int (Word.to_bitvec x) (Word.bitwidth x)
    | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
    | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
    | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
    | Ite (x,y,z) -> S.ite x y z
    | Extract (hi,lo,x) ->
      let s = max 0 (hi-lo+1) in
      S.extract s (byte hi) (byte lo) x
    | Concat (_,_) as cat -> S.concat (uncat [] cat)
    | Unknown (_, Imm s) -> S.unknown s
    | BinOp ((EQ|NEQ|LT|LE|SLT|SLE), _, _) as op ->
      S.ite op (Int Word.b1) (Int Word.b0)

    (* ill-formed expressions *)
    | Let _
    | Store (_, _, _, _, _)
    | Unknown (_, (Mem _|Unk)) as exp -> fail exp `Bitv; S.error



let mem : type t. (t,exp) mem_parser =
  fun (module S) -> function
    | Unknown (_,Mem (k,v)) ->
      S.unknown (Size.in_bits k) (Size.in_bits v)
    | Store (m,k,v,e,_) ->
      S.store_word (is_big e) m k v
    | Var v  ->
      let with_mem_types v f = match Var.typ v with
        | Mem (ks,vs) -> f (Size.in_bits ks) (Size.in_bits vs)
        | _ -> fail (Bil.Exp.Var v) `Mem; S.error in
      with_mem_types v (S.var (Var.name v))
    | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
    | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
    | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
    | Ite (c,x,y) ->  S.ite c x y
    (* the rest is ill-formed *)
    | Let _
    | Unknown (_,_)
    | Load (_,_,_,_)
    | BinOp (_,_,_)
    | UnOp (_,_)
    | Int _
    | Cast (_,_,_)
    | Extract (_,_,_)
    | Concat (_,_) as exp -> fail exp `Mem; S.error

let float _ _ = assert false
let rmode _ _ = assert false

let bool : type t r. (t,exp,r) bool_parser =
  fun (module S) -> function
    | Var x -> S.var (Var.name x)
    | Int x -> S.int (Word.to_bitvec x)
    | Cast (HIGH,1,x) -> S.high x
    | Cast (LOW,1,x) -> S.low x
    | BinOp (EQ,x,y) -> S.eq x y
    | BinOp (NEQ,x,y) -> S.neq x y
    | BinOp (LT,x,y) -> S.lt x y
    | BinOp (LE,x,y) -> S.le x y
    | BinOp (SLT,x,y) -> S.slt x y
    | BinOp (SLE,x,y) -> S.sle x y
    | BinOp (OR,x,y) -> S.logor x y
    | BinOp (AND,x,y) -> S.logand x y
    | BinOp (XOR,x,y) -> S.logxor x y
    | UnOp (NOT,x) -> S.not x
    | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
    | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
    | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
    | Ite (x,y,z) -> S.ite x y z
    | Extract (hi,lo,x) when hi = lo -> S.extract hi x
    | Unknown (_,_) -> S.unknown ()
    | Let _
    | Extract _
    | UnOp (NEG,_)
    | Cast (_,_,_)
    | Load (_,_,_,_)
    | Store (_,_,_,_,_)
    | Concat (_,_)
    | BinOp ((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|
              MOD|SMOD|LSHIFT|RSHIFT|ARSHIFT),_,_) as exp
      -> fail exp `Bool; S.error


let stmt : type t r. (t,exp,r,stmt) stmt_parser =
  fun (module S) ->
  let set v x =
    let n = Var.name v in
    match Var.typ v with
    | Unk ->
      error "can't reify the variable %s: unknown type" (Var.name v);
      S.error
    | Imm 1 -> S.set_bit n x
    | Imm m -> S.set_reg n m x
    | Mem (ks,vs) ->
      S.set_mem n (Size.in_bits ks) (Size.in_bits vs) x in
  fun s -> match Stmt.(decode call s) with
    | Some dst -> S.call dst
    | None -> match Stmt.(decode intrinsic s) with
      | Some data -> S.call data
      | None -> match s with
        | Move (v,x) -> set v x
        | Jmp (Int x) -> S.goto (Word.to_bitvec x)
        | Jmp x -> S.jmp x
        | Special s -> S.special s
        | While (c,xs) -> S.while_ c xs
        | If (c,xs,ys) -> S.if_ c xs ys
        | CpuExn n -> S.cpuexn n

let t = {bitv; mem; stmt; bool; float; rmode}
