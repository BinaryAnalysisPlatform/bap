open Bap_knowledge
open Bap_core_theory_definition
open Bap_core_theory_sort

open Knowledge.Syntax


let bool = Bool.t
let sort x = x >>| Value.sort

module Core : Core = struct
  type 'a t = 'a Knowledge.t
  let empty x = Knowledge.return @@ Value.empty x
  let data  = Knowledge.return @@ Eff.empty Kind.data
  let ctrl  = Knowledge.return @@ Eff.empty Kind.ctrl
  let unit  = Knowledge.return @@ Eff.empty Kind.unit

  let var v = empty (Var.sort v)
  let int s _ = empty s
  let unk s = empty s
  let b0 = empty bool
  let b1 = empty bool
  let inv _ = empty bool
  let and_ _ _ = empty bool
  let or_ _ _ = empty bool
  let msb _ = empty bool
  let lsb _ = empty bool
  let neg x = sort x >>= empty
  let not x = sort x >>= empty
  let add x _ = sort x >>= empty
  let sub x _ = sort x >>= empty
  let mul x _ = sort x >>= empty
  let div x _ = sort x >>= empty
  let sdiv x _ = sort x >>= empty
  let modulo x _ = sort x >>= empty
  let smodulo x _ = sort x >>= empty
  let logand x _ = sort x >>= empty
  let logor x _ = sort x >>= empty
  let logxor x _ = sort x >>= empty
  let shiftr _ x _ = sort x >>= empty
  let shiftl _ x _ = sort x >>= empty
  let ite _ x _ = sort x >>= empty
  let sle _ _ = empty bool
  let ule _ _ = empty bool
  let cast s _ _ = empty s
  let concat s _ = empty s
  let append s _ _ = empty s
  let load m _ = sort m >>| Mems.vals >>= empty
  let store m _ _ = sort m >>= empty
  let pass = data
  let skip = ctrl
  let set _ _ = data
  let let_ _ _ x = sort x >>= empty
  let jmp _ = ctrl
  let goto _ = ctrl
  let seq x _ = x >>| Eff.kind >>| Eff.empty
  let blk _ _ _ = unit
  let repeat _ _ = data
  let branch _ x _ = x >>| Eff.kind >>| Eff.empty
  let atomic _ = data
  let mfence = data
  let lfence = data
  let sfence = data

  let zero = empty
  let is_zero _ = empty bool
  let non_zero _ = empty bool
  let succ x = sort x >>= empty
  let pred x = sort x >>= empty
  let nsucc x _ = sort x >>= empty
  let npred x _ = sort x >>= empty
  let high s _ = empty s
  let low s _ = empty s
  let signed s _ = empty s
  let unsigned s _ = empty s
  let extract s _ _ _ = empty s
  let loadw s _ _ _ = empty s
  let storew _ x _ _ = sort x >>= empty
  let arshift x _ = sort x >>= empty
  let rshift x _ = sort x >>= empty
  let lshift x _ = sort x >>= empty

  let eq  _ _ = empty bool
  let neq _ _ = empty bool
  let slt _ _ = empty bool
  let ult _ _ = empty bool
  let sgt _ _ = empty bool
  let ugt _ _ = empty bool
  let sge _ _ = empty bool
  let uge _ _ = empty bool

  let rne = empty Rmode.t
  let rna = empty Rmode.t
  let rtp = empty Rmode.t
  let rtn = empty Rmode.t
  let rtz = empty Rmode.t
  let requal _ _  = empty bool

  let float s _ = empty s
  let fbits x = sort x >>| Floats.size >>= empty

  let is_finite _ = empty bool
  let is_fzero _ = empty bool
  let is_fneg _ = empty bool
  let is_fpos _ = empty bool
  let is_nan _ = empty bool
  let is_inf _ = empty bool
  let cast_float s _ _ = empty s
  let cast_sfloat s _ _ = empty s
  let cast_int s _ _ = empty s
  let cast_sint s _ _ = empty s
  let fneg x = sort x >>= empty
  let fabs x = sort x >>= empty
  let fadd _ x _ = sort x >>= empty
  let fsub _ x _ = sort x >>= empty
  let fmul _ x _ = sort x >>= empty
  let fdiv _ x _ = sort x >>= empty
  let fsqrt _ x = sort x >>= empty
  let fmodulo _ x _ = sort x >>= empty
  let fmad _ x _ _ = sort x >>= empty
  let fround _ x = sort x >>= empty
  let fconvert s _ _ = empty s
  let fsucc x = sort x >>= empty
  let fpred x = sort x >>= empty
  let forder _ _ = empty bool

  let pow _ x _ = sort x >>= empty
  let powr _ x _ = sort x >>= empty
  let compound _ x _ = sort x >>= empty
  let rootn _ x _ = sort x >>= empty
  let pownn _ x _ = sort x >>= empty
  let rsqrt _ x  = sort x >>= empty
  let hypot _ x _ = sort x >>= empty

  let exp _ x  = sort x >>= empty
  let expm1 _ x  = sort x >>= empty
  let exp2 _ x  = sort x >>= empty
  let exp2m1 _ x  = sort x >>= empty
  let exp10 _ x  = sort x >>= empty
  let exp10m1 _ x  = sort x >>= empty
  let log _ x  = sort x >>= empty
  let log2 _ x  = sort x >>= empty
  let log10 _ x  = sort x >>= empty
  let logp1 _ x  = sort x >>= empty
  let log2p1 _ x  = sort x >>= empty
  let log10p1 _ x  = sort x >>= empty
  let sin _ x  = sort x >>= empty
  let cos _ x  = sort x >>= empty
  let tan _ x  = sort x >>= empty
  let sinpi _ x  = sort x >>= empty
  let cospi _ x  = sort x >>= empty
  let atanpi _ x  = sort x >>= empty
  let atan2pi _ x _  = sort x >>= empty
  let asin _ x  = sort x >>= empty
  let acos _ x  = sort x >>= empty
  let atan _ x  = sort x >>= empty
  let atan2 _ x _ = sort x >>= empty
  let sinh _ x  = sort x >>= empty
  let cosh _ x  = sort x >>= empty
  let tanh _ x  = sort x >>= empty
  let asinh _ x  = sort x >>= empty
  let acosh _ x  = sort x >>= empty
  let atanh _ x  = sort x >>= empty
end
