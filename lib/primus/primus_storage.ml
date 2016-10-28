open Bap.Std
open Core_kernel.Std
open Monads.Std
open Primus_types



type cell =
  | Addr of addr
  | Var  of var


module type S = sig
  type ('a,'e) m constraint 'e = #Context.t
  val load : cell -> (word option,'e) m
  val save : cell -> (unit,'e) m
end

module type T = functor (M : Monad.State.Multi.S2) -> S with type ('a,'e) m = ('a,'e) M.t
type t = (module T)



module Null(SM : Monad.State.Multi.S2) = struct
  type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Context.t
  let load _ = SM.return None
  let save _ = SM.return ()
end

module Basic(SM : Machine) = struct
  type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Context.t
  type storage = {
    regs : word Var.Map.t;
    vars : word Var.Map.t;
    data : word Addr.Map.t;
  }

  let state = SM.Local.create ~name:"basic.storage" (fun ctxt -> {
        regs = Var.Map.empty;
        vars = Var.Map.empty;
        data = Addr.Map.empty;
      })

  type t = storage option
end

let null : t = (module Null)
