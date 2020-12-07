[@@@warning "-40"]

open Core_kernel
open Bap_knowledge
open Bap_core_theory
module Bil = X86_legacy_bil

open Bil
open X86_legacy_bil_pp

type exp = Ast.exp

type stmt = Ast.stmt
module Var = Var
type rmode = Type.roundmode_type

module Binary = X86_legacy_bil_semantics.Binary
include Bap.Std.Self()



let bits_of_var (_,_,t) = match t with
  | Type.Reg x -> x
  | _ -> invalid_arg "expects a bitvector"

let byte x = Ast.Int (Z.of_int x, Reg 8)


let word_of_z width z = Bitvec.(bigint z mod modulus width)
let sexp_of_prog = Bil.Ast.sexp_of_exp


let nan lift ieee x = lift ieee (Binary.mk_nan ieee x)

let var_name _ v = match String.chop_prefix ~prefix:"T_" v with
  | Some v -> v
  | None -> match String.chop_prefix ~prefix:"R_" v with
    | Some v -> v
    | None -> match v with
      | "mem64" | "mem32" -> "mem"
      | v -> v

let is_tmp v = String.is_prefix v ~prefix:"T_"

let type_error exp ctxt =
  warning "Ill-formed expression in the %s context: %s"
    ctxt (ast_exp_to_string exp)

let bitv : type t. (t,exp,rmode) Theory.Parser.bitv_parser =
  fun (module S) -> function
    | Cast (CAST_HIGH,Reg n,x) -> S.high n x
    | Cast (CAST_LOW,Reg n,x) -> S.low n x
    | Cast (CAST_UNSIGNED,Reg n,x) -> S.unsigned n x
    | Cast (CAST_SIGNED,Reg n,x) -> S.signed n x
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
    | Load(m,k,e,Reg s) -> S.load_word s e m k
    | Var (V (i,p,Reg s)) -> S.var (var_name i p) s
    | Int (z,Reg w) -> S.int (word_of_z w z) w
    | Let (V (i,p,Reg 1),y,z) -> S.let_bit (var_name i p) y z
    | Let (V (i,p,Reg _),y,z) -> S.let_reg (var_name i p) y z
    | Let (V (i,p,TMem _),y,z) -> S.let_mem (var_name i p) y z
    | Ite (x,y,z) -> S.ite x y z
    | Extract (hi,lo,x) ->
      let hi = Z.to_int hi and lo = Z.to_int lo in
      let s = max 0 (hi-lo+1) in
      S.extract s (byte hi) (byte lo) x
    | Concat (x,y) -> S.append x y
    | UnOp (FP (FFTOUBV s,r), x) -> S.cast_int s r x
    | UnOp (FP (FFTOSBV s,r), x) -> S.cast_sint s r x
    | UnOp (FP (FFTOIEEEBV n,_), x) ->
      S.fbits x

    | Unknown (_, Reg n) -> S.unknown n

    (* ill-formed expressions *)
    | Unknown _
    | BinOp ((EQ|NEQ|LT|LE|SLT|SLE), _, _)
    | Store (_, _, _, _, _)
    | BinOp (FP (_,_),_,_)
    | Var _
    | Load _
    | UnOp _ | Lab _
    | Cast _ | Int _ | Let _ as exp ->
      type_error exp "bitv";
      S.error


let mem : type t. (t,exp) Theory.Parser.mem_parser =
  fun (module S) -> function
    | Unknown (_,TMem (Reg k,Reg v)) -> S.unknown k v
    | Store (m,k,v,e,_) -> S.store_word e m k v
    | Var (V (i,n,TMem (Reg k, Reg v))) -> S.var (var_name i n) k v
    | Let (V (i,p,Reg 1),y,z) -> S.let_bit (var_name i p) y z
    | Let (V (i,p,Reg _),y,z) -> S.let_reg (var_name i p) y z
    | Let (V (i,p,TMem _),y,z) -> S.let_mem (var_name i p) y z
    | Ite (c,x,y) ->  S.ite c x y
    (* the rest is ill-formed *)
    | Unknown (_,_)
    | Load (_,_,_,_)
    | BinOp (_,_,_)
    | UnOp (_,_)
    | Int _
    | Cast (_,_,_)
    | Extract (_,_,_)
    | Concat (_,_)
    | Let _
    | Lab _
    | Var _ as exp -> type_error exp "mem"; S.error

let float : type t. (t,exp,rmode) Theory.Parser.float_parser =
  fun (module S) -> function
    | BinOp (FP (FADD,r),x,y) -> S.fadd r x y
    | BinOp (FP (FSUB,r),x,y) -> S.fsub r x y
    | BinOp (FP (FMUL,r),x,y) -> S.fmul r x y
    | BinOp (FP (FDIV,r),x,y) -> S.fdiv r x y
    | BinOp (FP (FREM,r),x,y) -> S.frem r x y
    | BinOp (FP (FMIN,_),x,y) -> S.fmin x y
    | BinOp (FP (FMAX,_),x,y) -> S.fmax x y
    | UnOp (FP (FABS,_),x) -> S.fabs x
    | UnOp (FP (FNEG,_),x) -> S.fabs x
    | UnOp (FP (FSQRT,r),x) -> S.fsqrt r x
    | UnOp (FP (FROUND,r),x) -> S.fround r x

    | UnOp (FP (FBVTOUF {exp_bits=5; sig_bits=11},r),x) ->
      S.ieee754_cast Theory.IEEE754.binary16 r x
    | UnOp (FP (FBVTOUF {exp_bits=8; sig_bits=24},r),x) ->
      S.ieee754_cast Theory.IEEE754.binary32 r x
    | UnOp (FP (FBVTOUF {exp_bits=11; sig_bits=53},r),x) ->
      S.ieee754_cast Theory.IEEE754.binary64 r x
    | UnOp (FP (FBVTOUF {exp_bits=15; sig_bits=64},r),x) ->
      S.ieee754_cast Theory.IEEE754.binary80 r x
    | UnOp (FP (FBVTOUF {exp_bits=15; sig_bits=113},r),x) ->
      S.ieee754_cast Theory.IEEE754.binary128 r x

    | UnOp (FP (FBVTOSF {exp_bits=5; sig_bits=11},r),x) ->
      S.ieee754_cast_signed Theory.IEEE754.binary16 r x
    | UnOp (FP (FBVTOSF {exp_bits=8; sig_bits=24},r),x) ->
      S.ieee754_cast_signed Theory.IEEE754.binary32 r x
    | UnOp (FP (FBVTOSF {exp_bits=11; sig_bits=53},r),x) ->
      S.ieee754_cast_signed Theory.IEEE754.binary64 r x
    | UnOp (FP (FBVTOSF {exp_bits=15; sig_bits=64},r),x) ->
      S.ieee754_cast_signed Theory.IEEE754.binary80 r x
    | UnOp (FP (FBVTOSF {exp_bits=15; sig_bits=113},r),x) ->
      S.ieee754_cast_signed Theory.IEEE754.binary128 r x

    | UnOp (FP (FFTOF {exp_bits=5; sig_bits=11},r),x) ->
      S.ieee754_convert Theory.IEEE754.binary16 r x
    | UnOp (FP (FFTOF {exp_bits=8; sig_bits=24},r),x) ->
      S.ieee754_convert Theory.IEEE754.binary32 r x
    | UnOp (FP (FFTOF {exp_bits=11; sig_bits=53},r),x) ->
      S.ieee754_convert Theory.IEEE754.binary64 r x
    | UnOp (FP (FFTOF {exp_bits=15; sig_bits=64},r),x) ->
      S.ieee754_convert Theory.IEEE754.binary80 r x
    | UnOp (FP (FFTOF {exp_bits=15; sig_bits=113},r),x) ->
      S.ieee754_convert Theory.IEEE754.binary128 r x

    | UnOp (FP (FIEEEBVTOF {exp_bits=5; sig_bits=11},r),x) ->
      S.ieee754 Theory.IEEE754.binary16 x
    | UnOp (FP (FIEEEBVTOF {exp_bits=8; sig_bits=24},r),x) ->
      S.ieee754 Theory.IEEE754.binary32 x
    | UnOp (FP (FIEEEBVTOF {exp_bits=11; sig_bits=53},r),x) ->
      S.ieee754 Theory.IEEE754.binary64 x
    | UnOp (FP (FIEEEBVTOF {exp_bits=15; sig_bits=64},r),x) ->
      S.ieee754 Theory.IEEE754.binary80 x
    | UnOp (FP (FIEEEBVTOF {exp_bits=15; sig_bits=113},r),x) ->
      S.ieee754 Theory.IEEE754.binary128 x


    | UnOp (FP (FNAN {exp_bits=5; sig_bits=11},r),x) ->
      nan S.ieee754 Theory.IEEE754.binary16 x
    | UnOp (FP (FNAN {exp_bits=8; sig_bits=24},r),x) ->
      nan S.ieee754 Theory.IEEE754.binary32 x
    | UnOp (FP (FNAN {exp_bits=11; sig_bits=53},r),x) ->
      nan S.ieee754 Theory.IEEE754.binary64 x
    | UnOp (FP (FNAN {exp_bits=15; sig_bits=64},r),x) ->
      nan S.ieee754 Theory.IEEE754.binary80 x
    | UnOp (FP (FNAN {exp_bits=15; sig_bits=113},r),x) ->
      nan S.ieee754 Theory.IEEE754.binary128 x

    | Ite (c,x,y) -> S.ite c x y
    | Load _ | Store _
    | UnOp _ | Var _ | Lab _
    | Int _ | Cast _ | Let _
    | Unknown _
    | Extract _
    | Concat _
    | BinOp _ as exp -> type_error exp "float"; S.error

let rmode : type t. (t,rmode) Theory.Parser.rmode_parser =
  fun (module S) -> function
    | RNE -> S.rne
    | RTZ -> S.rtz
    | RTP -> S.rtp
    | RTN -> S.rtn
    | RNA -> S.rna

let bool : type t. (t,exp,rmode) Theory.Parser.bool_parser =
  fun (module S) -> function
    | Var (V (i,v,Reg 1)) -> S.var (var_name i v)
    | Int (z,Reg 1) -> S.int (word_of_z 1 z)
    | Cast (CAST_HIGH,Reg 1,x) -> S.high x
    | Cast (CAST_LOW,Reg 1,x) -> S.low x
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
    | Let (V (i,p,Reg 1),y,z) -> S.let_bit (var_name i p) y z
    | Let (V (i,p,Reg _),y,z) -> S.let_reg (var_name i p) y z
    | Let (V (i,p,TMem _),y,z) -> S.let_mem (var_name i p) y z
    | Ite (x,y,z) -> S.ite x y z
    | Extract (hi,lo,x) when Z.equal hi lo -> S.extract (Z.to_int hi) x
    | Unknown (_,_) -> S.unknown ()
    | BinOp (FP (FLE, _), x, y) -> S.fle x y
    | BinOp (FP (FLT, _), x, y) -> S.flt x y
    | BinOp (FP (FEQ, _), x, y) -> S.feq x y
    | UnOp (FP (FISZERO,_), x) -> S.is_fzero x
    | UnOp (FP (FISINF,_), x) -> S.is_inf x
    | UnOp (FP (FISNAN,_), x) -> S.is_nan x
    | UnOp (FP (FISNEG,_), x) -> S.is_fneg x
    | UnOp (FP (FISPOS,_), x) -> S.is_fpos x
    | UnOp (FP ((FISNORM|FISSUB),_),x) -> S.unknown ()
    (* the rest is ill-formed *)
    | UnOp (_,_)
    | Cast (_,_,_)
    | Load (_,_,_,_)
    | Store (_,_,_,_,_)
    | Concat (_,_)
    | BinOp ((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|
              MOD|SMOD|LSHIFT|RSHIFT|ARSHIFT),_,_)
    | Var (V (_, _, Reg _))
    | Var (V (_, _, (TMem _|Array _|Float _)))
    | Let (V (_, _, (Array _|Float _)), _, _)
    | Lab _
    | Extract _
    | Int (_,_)
    | BinOp _ as exp -> type_error exp "bool"; S.error


let stmt : type t. (t,exp,rmode,stmt) Theory.Parser.stmt_parser =
  fun (module S) ->
  let set (Var.V (i,var,t)) x =
    let n = var_name i var in
    if is_tmp var
    then match t with
      | Reg 1 -> S.tmp_bit n x
      | Reg m -> S.tmp_reg n x
      | TMem (Reg ks,Reg vs) -> S.tmp_mem n x
      | Float {exp_bits=5; sig_bits=11} -> S.set_ieee754 n Theory.IEEE754.binary16 x
      | Float {exp_bits=8; sig_bits=24} -> S.set_ieee754 n Theory.IEEE754.binary32 x
      | Float {exp_bits=11; sig_bits=53} -> S.set_ieee754 n Theory.IEEE754.binary64 x
      | Float {exp_bits=15; sig_bits=64} -> S.set_ieee754 n Theory.IEEE754.binary80 x
      | Float {exp_bits=15; sig_bits=113} -> S.set_ieee754 n Theory.IEEE754.binary128 x
      | _ -> S.special "unsupported-type"
    else match t with
      | Reg 1 -> S.set_bit n x
      | Reg m -> S.set_reg n m x
      | TMem (Reg ks,Reg vs) -> S.set_mem n ks vs x
      | Float {exp_bits=5; sig_bits=11} -> S.set_ieee754 n Theory.IEEE754.binary16 x
      | Float {exp_bits=8; sig_bits=24} -> S.set_ieee754 n Theory.IEEE754.binary32 x
      | Float {exp_bits=11; sig_bits=53} -> S.set_ieee754 n Theory.IEEE754.binary64 x
      | Float {exp_bits=15; sig_bits=64} -> S.set_ieee754 n Theory.IEEE754.binary80 x
      | Float {exp_bits=15; sig_bits=113} -> S.set_ieee754 n Theory.IEEE754.binary128 x
      | _ -> S.special "unsupported-type" in
  function
  | Move (v,x,_) -> set v x
  | Jmp (x,_) -> S.jmp x
  | CJmp (c,x,a) -> S.if_ c [Jmp (x,a)] []
  | Special (s,_,_) -> S.special s
  | Assert _ -> S.special "assert" (* will add those later *)
  | Assume _ -> S.special "assume"
  | Comment _ -> S.seq []
  | Label _ -> S.special "label"
  | Halt _ -> S.special "halt"

let grammar = {Theory.Parser.bitv; mem; stmt; bool; float; rmode}


module Bap_integration = struct
  open Bap.Std
  include Self()

  open Bap_knowledge
  open Bap_core_theory
  open Knowledge.Syntax

  let bytes_of_mem mem =
    Bigsubstring.to_string (Memory.to_buffer mem)

  let addr_of_mem mem =
    Addr.to_int64_exn (Memory.min_addr mem)


  let fp_lifter = X86_legacy_fp_lifter.run


  let fixrip next prog =
    let rec stmt : Ast.stmt -> Ast.stmt = function
      | Move (v,x,ts) -> Move (v, exp x,ts)
      | Jmp (x,ts) -> Jmp (exp x, ts)
      | CJmp (x,y,ts) -> CJmp (exp x, exp y, ts)
      | Halt (x,ts) -> Halt (x,ts)
      | Assert (x,ts) -> Assert (x,ts)
      | Assume (x,ts) -> Assume (x,ts)
      | Label _
      | Comment _ | Special _ as x -> x
    and exp = function
      | Load (x,y,z,t) -> Load (exp x, exp y, exp z, t)
      | Store (a,b,c,d,t) -> Store (exp a, exp b, exp c, exp d, t)
      | BinOp (t,x,y) -> BinOp(t, exp x, exp y)
      | UnOp (t,x) -> UnOp (t, exp x)
      | Cast (t,s,x) -> Cast (t,s,exp x)
      | Let (v,x,y) -> Let (v, exp x, exp y)
      | Ite (x,y,z) -> Ite (exp x, exp y, exp z)
      | Concat (x,y) -> Concat (exp x, exp y)
      | Extract (x,y,z) -> Extract (x, y, exp z)
      | Lab _ | Int _ | Unknown _ as x -> x
      | Var (V (i,p,_)) as var -> match var_name i p with
        | "RIP" -> next
        | _ -> var in
    List.map prog ~f:stmt

  let empty = Theory.Semantics.empty

  let lift (module Core : Theory.Core) arch mem insn =
    match fp_lifter arch mem insn with
    | exception exn ->
      warning "failed with an exception: %a" Exn.pp exn;
      Knowledge.return empty
    | Error err ->
      debug "failed with an error: %a" Error.pp err;
      Knowledge.return empty
    | Ok prog ->
      let module Parser = Theory.Parser.Make(Core) in
      let pc = Addr.succ (Memory.max_addr mem) in
      let next = Ast.Int (Bitvec.to_bigint @@ Word.to_bitvec pc,
                          Reg (Word.bitwidth pc)) in
      let prog = fixrip next prog in
      Parser.run grammar prog

  let (>>?) x f = x >>= function
    | None -> KB.return empty
    | Some x -> f x

  let provide_lifter () =
    info "providing a lifter for all Legacy AST lifters";
    let lifter obj =
      Knowledge.collect Arch.slot obj >>= fun arch ->
      Theory.instance ~context:["floating-point"] () >>=
      Theory.require >>= fun theory ->
      Knowledge.collect Memory.slot obj >>? fun mem ->
      Knowledge.collect Disasm_expert.Basic.Insn.slot obj >>? fun insn ->
      match arch with
      | `x86 | `x86_64 as arch ->
        KB.catch (lift theory arch mem insn)
          (fun _ -> !!empty)
      | _ -> Knowledge.return empty in
    Knowledge.promise Theory.Semantics.slot lifter
end

let init () = Bap_integration.provide_lifter ()
