open Bap_core_theory
open Base
open KB.Syntax
(** This is mean to design a dsl for ARM lifter *)

(** General cpu defs. *)
module type CPU = sig
  type value
  type reg_type
  type operand
  val value: value Theory.Bitv.t Theory.Value.sort
  val load_reg: reg_type -> value Theory.Bitv.t Theory.Var.t
  val assert_var: operand -> value Theory.Bitv.t Theory.Var.t
  val assert_val: operand -> value Theory.Bitv.t Theory.pure
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

  let assert_var = CPU.assert_var

  let assert_val = CPU.assert_val

  let value_size = Theory.Bitv.size CPU.value

  let word_as_bitv w = (int CPU.value (Bap.Std.Word.to_bitvec w))

  let imm (x : int) = word_as_bitv (Bap.Std.Word.of_int value_size x)

  let ( := )  = set
  let set_if cond dest src = dest := ite cond src (var dest)
  let when_ (cond : bool) eff = 
    let bot = perform Theory.Effect.Sort.bot in
    if cond then eff |> expand else bot
  let ( < )  = slt
  let ( > )  = sgt
  let ( <= )  = sle
  let ( >= )  = sge
  let ( = )   = eq
  let ( <> )   = neq
  let ( << )  = lshift
  let ( >> )  = rshift
  let ( lor )  = logor
  let ( land ) = logand
  let ( lxor ) = logxor
  let ( lnot ) = not

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
    let ( - ) x y  = sub x y
    let ( * ) x y  = mul x y
    let ( / ) x y  = div x y
    let ( ^ ) x y  = concat x y
    let ( % ) x y  = modulo x y
  end

  include Arith

  let extend v = cast CPU.value b0 v
  let extend_to s v = cast s b0 v

end
