open Core_kernel
open Bap.Std
open Bap_future.Std
open Bap_knowledge
open Bap_core_theory

open Knowledge.Syntax
open Link.Syntax

open Parser
include Self()

module Grammar = struct
  type exp = Bil.exp
  module Var = Bap.Std.Var
  let rec uncat acc : exp -> exp list = function
    | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
    | Concat (x,y) -> x::y::acc
    | x -> x::acc

  let bits_of_var v = match Var.typ v with
    | Imm x -> x
    | _ -> invalid_arg "expects a bitvector"

  let byte x = Bil.int (Word.of_int ~width:8 x)
  let is_big e =
    Bil.int @@
    if e = BigEndian then Word.b1 else Word.b0

  let bitv : type t. (t,exp) bitv_parser =
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
      | Int x -> S.int x
      | Let (x,y,z) -> S.let_ (Var.name x) y z
      | Ite (x,y,z) -> S.ite x y z
      | Extract (hi,lo,x) ->
        let s = max 0 (hi-lo+1) in
        S.extract s (byte hi) (byte lo) x
      | Concat (_,_) as cat -> S.concat (uncat [] cat)
      (* ill-formed expressions *)
      | BinOp ((EQ|NEQ|LT|LE|SLT|SLE), _, _)
      | Store (_, _, _, _, _)
      | Unknown (_, _) -> assert false

  let with_mem_types v f = match Var.typ v with
    | Mem (ks,vs) -> f (Size.in_bits ks) (Size.in_bits vs)
    | _ -> assert false

  let mem : type t. (t,exp) mem_parser =
    fun (module S) -> function
      | Unknown (_,Mem (k,v)) ->
        S.unknown (Size.in_bits k) (Size.in_bits v)
      | Store (m,k,v,e,_) ->
        S.store_word (is_big e) m k v
      | Var v  -> with_mem_types v (S.var (Var.name v))
      | Let (v,x,y) -> S.let_ (Var.name v) x y
      | Ite (c,x,y) ->  S.ite c x y
      (* the rest is ill-formed *)
      | Unknown (_,_)
      | Load (_,_,_,_)
      | BinOp (_,_,_)
      | UnOp (_,_)
      | Int _
      | Cast (_,_,_)
      | Extract (_,_,_)
      | Concat (_,_) -> assert false

  let bool : type t. (t,exp) bool_parser =
    fun (module S) -> function
      | Var x -> S.var (Var.name x)
      | Int x -> S.int x
      | Cast (HIGH,n,x) -> S.high n x
      | Cast (LOW,n,x) -> S.low n x
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
      | Let (x,y,z) -> S.let_ (Var.name x) y z
      | Ite (x,y,z) -> S.ite x y z
      | Extract (hi,lo,x) -> S.extract hi lo x
      | Unknown (_,_) -> S.unknown ()

      (* the rest is ill-formed *)
      | UnOp (NEG,_)
      | Cast ((SIGNED|UNSIGNED),_,_)
      | Load (_,_,_,_)
      | Store (_,_,_,_,_)
      | Concat (_,_) -> assert false
      | BinOp ((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|
                MOD|SMOD|LSHIFT|RSHIFT|ARSHIFT),_,_)
        -> assert false


  let stmt : type t. (t,exp,stmt) stmt_parser =
    fun (module S) ->
      let set v x =
        let n = Var.name v in
        match Var.typ v with
        | Imm 1 -> S.set_bit n x
        | Imm m -> S.set_reg n m x
        | Mem (ks,vs) ->
          S.set_mem n (Size.in_bits ks) (Size.in_bits vs) x in
      let let_ v x =
        let n = Var.name v in
        match Var.typ v with
        | Imm 1 -> S.tmp_bit n x
        | Imm _ -> S.tmp_reg n x
        | Mem _ -> S.tmp_mem n x in
      function
      | Move (v,x) when Var.is_physical v -> set v x
      | Move (v,x) -> let_ v x
      | Jmp (Int x) -> S.goto x
      | Jmp x -> S.jmp x
      | Special s -> S.special s
      | While (c,xs) -> S.while_ c xs
      | If (c,xs,ys) -> S.if_ c xs ys
      | CpuExn n -> S.cpuexn n

  let t = {bitv; mem; stmt; bool}
end

module Parser = Parser.Make(Theory.Manager)

let provide_lifter arch =
  info "providing a lifter for arch %a" Arch.pp arch;
  let module Target = (val target_of_arch arch) in
  let lifter label =
    Knowledge.collect Disasm_expert.Basic.decoder label >>= function
    | None -> Knowledge.return Semantics.empty
    | Some (mem,insn) ->
      match Target.lift mem insn with
      | Error _ -> Knowledge.return Semantics.empty
      | Ok bil ->
        Parser.run Grammar.t bil >>| fun eff ->
        Eff.semantics eff in
  Knowledge.promise Insn.Semantics.t lifter


let init () =
  Stream.observe Project.Info.arch provide_lifter
