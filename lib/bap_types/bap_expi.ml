open Core_kernel.Std
open Monads.Std
open Bap_result
open Bap_expi_types

module type S = Expi.S

class context = object(self)
  inherit Bap_context.t
  val id = Id.(succ zero)
  method private next = {< id = Id.succ id >}
  method create_undefined = self#next, undefined id
  method create_word u = self#next, word u id
  method create_storage s = self#next, storage s id
end

module Make(State : Monad.State.S2) = struct
  open State.Syntax

  type ('a,'e) state = ('a,'e) State.t
  type 'a u = (unit,'a) state
  type 'a r = (Bap_result.result,'a) state

  module Eval = Bap_eval.Make(State)

  let create_result f =
    State.get () >>= fun ctxt ->
    let (ctxt,v) = f ctxt in
    State.put ctxt >>= fun () ->
    State.return v

  let undefined () =
    create_result (fun ctxt -> ctxt#create_undefined)

  let word w =
    create_result (fun ctxt -> ctxt#create_word w)

  let storage s =
    create_result (fun ctxt -> ctxt#create_storage s)

  class ['a] t = object(self)
    constraint 'a = #context
    inherit ['a,result,storage] Eval.t

    method empty = new Storage.linear
    method private undefined = undefined ()
    method undefined_var v = self#undefined
    method undefined_addr a = self#undefined
    method private value_of_word = word
    method private word_of_value v = match value v with
      | Imm x -> State.return (Some x)
      | _ -> State.return None
    method private storage_of_value v = match value v with
      | Mem mem -> State.return (Some mem)
      | _ -> State.return None
    method private value_of_storage = storage

    method lookup v =
      State.get () >>= fun ctxt -> match ctxt#lookup v with
      | Some v -> State.return v
      | None -> self#undefined_var v

    method update var data  =
      State.update @@ fun s -> s#update var data

    method load mem addr = match mem#load addr with
        | None -> self#undefined_addr addr
        | Some w -> word w

    method store mem addr word =
      storage (mem#save addr word)
  end
end

include Make(Monad.State)
