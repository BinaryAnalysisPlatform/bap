open Bap_core_theory
open Base
open KB.Syntax
(** This is mean to design a dsl for ARM lifter *)

(** General cpu defs. *)
module type CPU = sig
  type value
  type bit_val
  type reg_type
  type operand
  val value: value Theory.Bitv.t Theory.Value.sort
  val bit_val: bit_val Theory.Bitv.t Theory.Value.sort
  val load_reg: reg_type -> value Theory.Bitv.t Theory.Var.t
  val assert_var: operand -> value Theory.Bitv.t Theory.Var.t
  val assert_val: operand -> value Theory.Bitv.t Theory.pure
end

(** General fpu defs *)
module type FPU = sig
  type operand
  type value
  val value : value Theory.Float.t Theory.Value.sort
  val assert_var: operand -> value Theory.Float.t Theory.Var.t
  val assert_val: operand -> value Theory.Float.t Theory.pure
end

module type ValueHolder = sig
  type value
  val value: value Theory.Bitv.t Theory.Value.sort
end

module DSL(Core: Theory.Core)(CPU: CPU)(V: ValueHolder) = struct
  open Core

  let expand (dsl : 'a Theory.eff list) = 
    let bot = perform Theory.Effect.Sort.bot in
    List.fold dsl ~init:bot 
      ~f:(fun acc current -> seq acc current)

  (** alternative of expand to enable short syntax *)
  let ( !% ) = expand

  let assert_var = CPU.assert_var

  let (!$$) = assert_var

  let assert_val = CPU.assert_val

  let (!$) = assert_val

  let value_size = Theory.Bitv.size CPU.value

  let word_as_bitv w = (int CPU.value (Bap.Std.Word.to_bitvec w))

  let bool_as_bitv b = ite b (int CPU.value Bitvec.one) (int CPU.value Bitvec.zero)

  let bool_as_bit b = ite b (int CPU.bit_val Bitvec.one) (int CPU.bit_val Bitvec.zero)

  let imm (x : int) = word_as_bitv (Bap.Std.Word.of_int value_size x)

  let (!!) = imm

  let ( := )  = set
  let set_if cond dest src = dest := ite cond src (var dest)
  let when_else_ (cond : bool) then_ else_ =
    if cond then then_ |> expand else else_ |> expand
  let when_ (cond : bool) eff =
    let bot = perform Theory.Effect.Sort.bot in
    when_else_ cond eff [bot]
  let unless_ (cond : bool) eff = when_ (phys_equal cond false) eff
  let if_else_ (cond : Theory.bool) then_ else_ =
    branch cond (then_ |> expand) (else_ |> expand)
  let if_ (cond : Theory.bool) then_ =
    let bot = perform Theory.Effect.Sort.bot in
    if_else_ cond then_ [bot]

  (** this is a static effect list generator (from core theory perspective) *)
  let rec while_ (cond : 'a -> bool * 'a) (init : 'a) (perf : 'a -> 'b Theory.eff) =
    match cond init with
    | true, next -> [perf init] @ while_ cond next perf
    | false, _ -> [perform Theory.Effect.Sort.bot]

  let foreach_ (list : 'a list) (perf : 'a -> int -> 'b Theory.eff) =
    List.foldi list ~init:(perform Theory.Effect.Sort.bot)
      ~f:(fun it eff item -> seq eff (perf item it))

  let ( < )  = slt
  let ( > )  = sgt
  let ( <= )  = sle
  let ( >= )  = sge
  let ( <+ ) = ult
  let ( >+ ) = ugt
  let ( <=+ ) = ule
  let ( >=+ ) = uge
  let ( = )   = eq
  let ( <> )   = neq
  let ( << )  = lshift
  let ( >> )  = rshift
  let ( asr ) = arshift
  let ( lor )  = logor
  let ( land ) = logand
  let ( lxor ) = logxor
  let ( lnot ) = not

  let nth_bit n e = cast CPU.bit_val b0 (shiftr b0 e n) |> msb

  module Bool = struct
    let ( lor ) = or_
    let ( land ) = and_
    let ( lnot ) = inv
    let ( lxor ) x y = (lnot x land y) lor (x land lnot y)
    let ( = ) = (lxor)
    let ( <> ) x y = lnot (x = y)
  end

  module Arith = struct
    let distribute op x y = op (cast V.value b0 x) (cast V.value b0 y)
    let ( + ) x y  = distribute add x y
    let ( - ) x y  = distribute sub x y
    let ( * ) x y  = distribute mul x y
    (* multiplication with signed cast *)
    let ( -* ) x y = mul (signed V.value x) (signed V.value y)
    let ( / ) x y  = distribute div x y
    let ( % ) x y  = distribute modulo x y
  end

  include Arith

  let extend v = cast CPU.value b0 v
  let extend_to s v = cast s b0 v
  let extend_signed v = signed CPU.value v
  let extend_to_signed s v = signed s v
  let scoped_var = Theory.Var.scoped
  let local_var = Theory.Var.fresh CPU.value
  let local_var_sort = Theory.Var.fresh

end

module DSLFP(Core : Theory.Core)(FPU: FPU) = struct
  let assert_var = FPU.assert_var

  let (!$$.) = assert_var

  let assert_val = FPU.assert_val

  let (!$.) = assert_val
end