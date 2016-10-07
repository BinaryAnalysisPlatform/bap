open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_result
open Bap_monad_types
open Bap_bili_types

module Sz = Bap_size
module TE = Bap_type_error
module Monad = Bap_monad

class context = object
  inherit Bap_expi.context
  val pc  = Bot
  method pc = pc
  method with_pc pc = {< pc = pc >}
end

let imm_t imm = Type.Imm (Bitvector.bitwidth imm)
let bool_t = Type.Imm 1

module type S = Bili.S

module Make(SM : Monad.State.S) = struct
  open SM
  type ('a,'e) state = ('a,'e) SM.t
  type 'a u = (unit,'a) state
  type 'a r = (Bap_result.result,'a) state

  module Expi = Bap_expi.Make(SM)



  class ['a] t = object(self)
    constraint 'a = #context
    inherit ['a] Expi.t

    method private type_error' error : 'a u =
      self#type_error error >>| fun _ -> ()

    method private jump addr : 'a u =
      get () >>= fun s -> put @@ s#with_pc addr

    method eval (ss : stmt list) : 'a u =
      List.fold ss ~init:(return ()) ~f:(fun x s ->
          x >>= fun () -> self#eval_stmt s)

    method eval_stmt (s : stmt) : 'a u = match s with
      | Stmt.Move (v,u) -> self#eval_move v u
      | Stmt.Jmp e -> self#eval_jmp e
      | Stmt.Special s -> self#eval_special s
      | Stmt.While (cond,body) -> self#eval_while ~cond ~body
      | Stmt.If (cond,yes,no) -> self#eval_if ~cond ~yes ~no
      | Stmt.CpuExn u -> self#eval_cpuexn u

    method eval_cpuexn u = return ()

    method eval_move v u : 'a u =
      self#eval_exp u >>= fun u -> self#update v u


    method eval_jmp e : 'a u =
      self#eval_exp e >>| value >>= function
      | Mem v -> self#type_error' TE.bad_imm
      | dst -> self#jump dst

    method eval_special _ = return ()

    method eval_while ~cond ~body : 'a u =
      self#eval_exp cond >>| value >>= function
      | Bot -> return ()
      | Mem v -> self#type_error' TE.bad_imm
      | Imm r ->
        if Bitvector.(r = b0) then return () else
        if Bitvector.(r = b1)
        then self#eval body >>= fun () -> self#eval_while ~cond ~body
        else self#type_error' @@ TE.bad_type bool_t (imm_t r)

    method eval_if ~cond ~yes ~no : 'a u =
      self#eval_exp cond >>| value >>= function
      | Bot -> return ()
      | Mem v -> self#type_error' TE.bad_imm
      | Imm r ->
        if Bitvector.(r = b0) then self#eval no  else
        if Bitvector.(r = b1) then self#eval yes
        else self#type_error' @@ TE.bad_type bool_t (imm_t r)
  end
end

include Make(Monad.State)
