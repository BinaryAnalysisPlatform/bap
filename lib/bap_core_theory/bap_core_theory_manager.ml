open Core_kernel
open Bap.Std
open Bap_knowledge

open Bap_core_theory_definition
open Bap_core_theory_sort

open Knowledge.Syntax

let size = Bits.size
let sort x = x >>| Value.sort

type 'a t = {
  name : string;
  desc : string;
  proc : 'a;
}

type provider = (module Core)

let providers : provider t list ref = ref []
let register ?(desc="") ~name x =
  let provider = {name; desc; proc = x} in
  providers := provider :: !providers

let bool = Bool.t

module Tid = struct
  let t = Semantics.declare ~name:"tid" (module Domain.Label)
end

let ret = Knowledge.return

let newval s =
  Label.Generator.fresh >>| fun id ->
  Value.put Tid.t (Value.empty s) id

let neweff s =
  Label.Generator.fresh >>| fun id ->
  Eff.put Tid.t (Eff.empty s) id

let effect x = x >>| Eff.kind

let foreach f init = Knowledge.List.fold !providers ~init ~f

let lift0 gen join sort f =
  gen sort >>=
  foreach @@begin fun r {proc} ->
    f proc >>| fun r' ->
    join r r'
  end

let lift1 gen join x sort f =
  x >>= fun x ->
  sort !!x >>= gen >>=
  foreach @@begin fun r {proc} ->
    f proc !!x >>| fun r' ->
    join r r'
  end

let lift2 gen join x y sort f =
  x >>= fun x ->
  y >>= fun y ->
  sort !!x !!y >>= gen >>=
  foreach @@begin fun r {proc} ->
    f proc !!x !!y >>| fun r' ->
    join r r'
  end

let lift3 gen join x y z sort f =
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  sort !!x !!y !!z >>= gen >>=
  foreach @@begin fun r {proc} ->
    f proc !!x !!y !!z >>| fun r' ->
    join r r'
  end

let lift4 gen join x y z a sort f =
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  a >>= fun a ->
  sort !!x !!y !!z !!a >>= gen >>=
  foreach @@begin fun r {proc} ->
    f proc !!x !!y !!z !!a >>| fun r' ->
    join r r'
  end

let val0 sort f = lift0 newval Value.merge sort f
let val1 x sort f = lift1 newval Value.merge x sort f
let val2 x y sort f = lift2 newval Value.merge x y sort f
let val3 x y z sort f = lift3 newval Value.merge x y z sort f
let val4 x y z a sort f = lift4 newval Value.merge x y z a sort f
let eff0 sort f = lift0 neweff Eff.merge sort f
let eff1 x sort f = lift1 neweff Eff.merge x sort f
let eff2 x y sort f = lift2 neweff Eff.merge x y sort f
let eff3 x y z sort f = lift3 neweff Eff.merge x y z sort f

module Theory : Core = struct
  type 'a t = 'a Knowledge.t

  let var v = val0 (Var.sort v) @@ fun (module P) -> P.var v
  let int s x = val0 s @@ fun (module P) -> P.int s x
  let unk s = val0 s @@ fun (module P) -> P.unk s
  let b0 = val0 bool @@ fun (module P) -> P.b0
  let b1 = val0 bool @@ fun (module P) -> P.b1
  let inv x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.inv
  let and_ x y = val2 x y (fun _ _ -> !!bool) @@ fun (module P) -> P.and_
  let or_ x y = val2 x y (fun _ _ -> !!bool) @@ fun (module P) -> P.or_
  let msb x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.msb
  let lsb x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.lsb

  let neg x = val1 x sort @@ fun (module P) -> P.neg
  let not x = val1 x sort @@ fun (module P) -> P.not

  let uop x f = val1 x sort f
  let aop x y f = val2 x y (fun x _ -> sort x) f
  let add x y = aop x y @@ fun (module P) -> P.add
  let sub x y = aop x y @@ fun (module P) -> P.sub
  let mul x y = aop x y @@ fun (module P) -> P.mul
  let div x y = aop x y @@ fun (module P) -> P.div
  let sdiv x y = aop x y @@ fun (module P) -> P.sdiv
  let modulo x y = aop x y @@ fun (module P) -> P.modulo
  let smodulo x y = aop x y @@ fun (module P) -> P.smodulo
  let logand x y = aop x y @@ fun (module P) -> P.logand
  let logor x y = aop x y @@ fun (module P) -> P.logor
  let logxor x y = aop x y @@ fun (module P) -> P.logxor

  let shiftr b x y = val3 b x y (fun _ x _ -> sort x) @@
    fun (module P) -> P.shiftr
  let shiftl b x y = val3 b x y (fun _ x _ -> sort x) @@
    fun (module P) -> P.shiftl
  let ite b x y = val3 b x y (fun _ x _ -> sort x) @@
    fun (module P) -> P.ite

  let lop x y f = val2 x y  (fun _ _ -> !!bool) f
  let sle x y = lop x y @@ fun (module P) -> P.sle
  let ule x y = lop x y @@ fun (module P) -> P.ule

  let cast s x z = val2 x z (fun _ _ -> !!s) @@
    fun (module P) -> P.cast s

  let concat s xs =
    Knowledge.List.all xs >>= fun xs ->
    let xs = List.map ~f:(!!) xs in
    newval s >>=
    foreach @@begin fun r {proc=(module P)} ->
      P.concat s xs >>| fun r' ->
      Value.merge r r'
    end

  let append s x y = val2 x y (fun _ _ -> !!s) @@
    fun (module P) -> P.append s

  let load m k = val2 m k (fun m _ -> sort m >>| Mems.vals) @@
    fun (module P) -> P.load

  let store m k v = val3 m k v (fun m _ _ -> sort m) @@
    fun (module P) -> P.store

  let pass = eff0 Kind.data @@ fun (module P) -> P.pass
  let skip = eff0 Kind.ctrl @@ fun (module P) -> P.skip
  let set v x = eff1 x (fun _ -> !!Kind.data) @@ fun (module P) ->
    P.set v

  let let_ v x b = val2 x b (fun _ x -> sort x) @@ fun (module P) ->
    P.let_ v

  let jmp d = eff1 d (fun _ -> !!Kind.ctrl) @@ fun (module P) ->
    P.jmp

  let goto d = eff0 Kind.ctrl @@ fun (module P) -> P.goto d

  let seq x y = eff2 x y (fun x _ -> effect x) @@ fun (module P) ->
    P.seq
  let blk l x y = eff2 x y (fun _ _ -> !!Kind.unit) @@ fun (module P) ->
    P.blk l

  let repeat b x = eff2 b x (fun _ _ -> !!Kind.data) @@ fun (module P) ->
    P.repeat

  let branch b x y = eff3 b x y (fun _ x _ -> effect x) @@ fun (module P) ->
    P.branch

  let atomic x = eff1 x (fun _ -> !!Kind.data) @@ fun (module P) ->
    P.atomic
  let mfence = eff0 Kind.data @@ fun (module P) -> P.mfence
  let lfence = eff0 Kind.data @@ fun (module P) -> P.lfence
  let sfence = eff0 Kind.data @@ fun (module P) -> P.sfence

  (* Provider *)
  let zero s = val0 s @@ fun (module P) -> P.zero s
  let is_zero x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_zero
  let non_zero x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.non_zero
  let succ x = val1 x sort @@ fun (module P) -> P.succ
  let pred x = val1 x sort @@ fun (module P) -> P.pred
  let nsucc x n = val1 x sort @@ fun (module P) x ->
    P.nsucc x n
  let npred x n = val1 x sort @@ fun (module P) x ->
    P.npred x n

  let high s x = val1 x (fun _ -> !!s) @@ fun (module P) ->
    P.high s
  let low s x = val1 x (fun _ ->  !!s) @@ fun (module P) ->
    P.low s
  let signed s x = val1 x (fun _ -> !!s) @@ fun (module P) ->
    P.signed s
  let unsigned s x = val1 x (fun _ -> !!s) @@ fun (module P) ->
    P.unsigned s

  let extract s x y z = val3 x y z (fun _ _ _ -> !!s) @@ fun (module P) ->
    P.extract s

  let loadw s d m k = val3 d m k (fun _ _ _ -> !!s) @@ fun (module P) ->
    P.loadw s

  let storew d m k x = val4 d m k x (fun _ m _ _ -> sort m) @@ fun (module P) ->
    P.storew

  let arshift x y = val2 x y (fun x _ -> sort x) @@ fun (module P) ->
    P.arshift
  let rshift x y = val2 x y (fun x _ -> sort x) @@ fun (module P) ->
    P.rshift
  let lshift x y = val2 x y (fun x _ -> sort x) @@ fun (module P) ->
    P.lshift

  let eq x y = lop x y @@ fun (module P) -> P.eq
  let neq x y = lop x y @@ fun (module P) -> P.neq
  let slt x y = lop x y @@ fun (module P) -> P.slt
  let ult x y = lop x y @@ fun (module P) -> P.ult
  let sgt x y = lop x y @@ fun (module P) -> P.sgt
  let ugt x y = lop x y @@ fun (module P) -> P.ugt
  let sge x y = lop x y @@ fun (module P) -> P.sge
  let uge x y = lop x y @@ fun (module P) -> P.uge


  let rne = val0 Rmode.t @@ fun (module P) -> P.rne
  let rna = val0 Rmode.t @@ fun (module P) -> P.rne
  let rtp = val0 Rmode.t @@ fun (module P) -> P.rne
  let rtn = val0 Rmode.t @@ fun (module P) -> P.rne
  let rtz = val0 Rmode.t @@ fun (module P) -> P.rne
  let requal x y = val2 x y (fun _ _ -> !!bool) @@ fun (module P) ->
    P.requal

  let float s x = val1 x (fun _ -> !!s) @@ fun (module P) -> P.float s
  let fbits x = val1 x (fun x -> sort x >>| Floats.size) @@ fun (module P) ->
    P.fbits

  let is_finite x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_finite
  let is_fzero x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_fzero
  let is_fneg x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_fneg
  let is_fpos x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_fpos
  let is_nan x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_nan
  let is_inf x = val1 x (fun _ -> !!bool) @@ fun (module P) -> P.is_inf

  let cast_float s m x = val2 m x (fun _ _ -> !!s) @@ fun (module P) ->
    P.cast_float s

  let cast_sfloat s m x = val2 m x (fun _ _ -> !!s) @@ fun (module P) ->
    P.cast_sfloat s

  let cast_int s m x = val2 m x (fun _ _ -> !!s) @@ fun (module P) ->
    P.cast_int s

  let cast_sint s m x = val2 m x (fun _ _ -> !!s) @@ fun (module P) ->
    P.cast_sint s

  let fneg x = uop x @@ fun (module P) -> P.fneg
  let fabs x = uop x @@ fun (module P) -> P.fabs

  let faop m x y f = val3 m x y (fun _ x _ -> sort x) f
  let fadd m x y = faop m x y @@ fun (module P) -> P.fadd
  let fsub m x y = faop m x y @@ fun (module P) -> P.fsub
  let fmul m x y = faop m x y @@ fun (module P) -> P.fmul
  let fdiv m x y = faop m x y @@ fun (module P) -> P.fdiv
  let fmodulo m x y = faop m x y @@ fun (module P) -> P.fmodulo

  let fmad m x y z = val4 m x y z (fun _ x _ _ -> sort x) @@ fun (module P) ->
    P.fmad

  let fround m x = val2 m x (fun _ x -> sort x) @@ fun (module P) ->
    P.fround
  let fconvert s x y = val2 x y (fun _ _ -> !!s) @@ fun (module P) ->
    P.fconvert s

  let fsucc x = uop x @@ fun (module P) -> P.fsucc
  let fpred x = uop x @@ fun (module P) -> P.fpred

  let forder x y = val2 x y (fun _ _ -> !!bool) @@ fun (module P) -> P.forder

  let pow m x y = faop m x y @@ fun (module P) -> P.pow
  let powr m x y = faop m x y @@ fun (module P) -> P.powr


  let compound m x y = faop m x y @@ fun (module P) -> P.compound
  let rootn m x y = faop m x y @@ fun (module P) -> P.rootn
  let pownn m x y = faop m x y @@ fun (module P) -> P.pownn

  let fuop m x f = val2 m x (fun _ x -> sort x) f
  let fsqrt m x = fuop m x @@ fun (module P) -> P.fsqrt
  let rsqrt m x = fuop m x @@ fun (module P) -> P.rsqrt
  let hypot m x y = faop m x y @@ fun (module P) -> P.hypot

  let exp m x = fuop m x @@ fun (module P) -> P.exp
  let expm1 m x = fuop m x @@ fun (module P) -> P.expm1
  let exp2 m x = fuop m x @@ fun (module P) -> P.exp2
  let exp2m1 m x = fuop m x @@ fun (module P) -> P.exp2m1
  let exp10 m x = fuop m x @@ fun (module P) -> P.exp10
  let exp10m1 m x = fuop m x @@ fun (module P) -> P.exp10m1
  let log m x = fuop m x @@ fun (module P) -> P.log
  let log2 m x = fuop m x @@ fun (module P) -> P.log2
  let log10 m x = fuop m x @@ fun (module P) -> P.log10
  let logp1 m x = fuop m x @@ fun (module P) -> P.logp1
  let log2p1 m x = fuop m x @@ fun (module P) -> P.log2p1
  let log10p1 m x = fuop m x @@ fun (module P) -> P.log10p1
  let sin m x = fuop m x @@ fun (module P) -> P.sin
  let cos m x = fuop m x @@ fun (module P) -> P.cos
  let tan m x = fuop m x @@ fun (module P) -> P.tan
  let sinpi m x = fuop m x @@ fun (module P) -> P.sinpi
  let cospi m x = fuop m x @@ fun (module P) -> P.cospi
  let atanpi m x = fuop m x @@ fun (module P) -> P.atanpi
  let atan2pi m x y = faop m x y @@ fun (module P) -> P.atan2pi
  let asin m x = fuop m x @@ fun (module P) -> P.asin
  let acos m x = fuop m x @@ fun (module P) -> P.acos
  let atan m x = fuop m x @@ fun (module P) -> P.atan
  let atan2 m x y = faop m x y @@ fun (module P) -> P.atan2
  let sinh m x = fuop m x @@ fun (module P) -> P.sinh
  let cosh m x = fuop m x @@ fun (module P) -> P.cosh
  let tanh m x = fuop m x @@ fun (module P) -> P.tanh
  let asinh m x = fuop m x @@ fun (module P) -> P.asinh
  let acosh m x = fuop m x @@ fun (module P) -> P.acosh
  let atanh m x = fuop m x @@ fun (module P) -> P.atanh
end
