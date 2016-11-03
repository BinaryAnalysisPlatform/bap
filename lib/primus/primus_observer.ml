open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context
module Storage = Primus_storage

type cell = Storage.cell = Addr of addr | Var of var




module type Observer = sig
  type ('a,'e) m constraint 'e = #Context.t
end

module type S = sig
  type ('a,'e) m constraint 'e = #Context.t
  val step : ('p,'t) cls -> 't term -> (unit,'e) m

  (* each call to observe creates a new value.
     an observer must not create new contexts (we should enforce this
     by providing only a deterministic machine interface).

     How to tell, that observer has observed all possible values.
 *)
  val observe : cell -> (word,'e) m

  (** [coverage cell] is a value between [0] and [1], that shows a
      percentage (deterministically or probobalistically) of an area
      of the value domain, that was observed.


      Alternatively:

      val entropy : cell -> (float,'e) m
*)
  val coverage : cell -> (float,'e) m


  (** [observed cell] is true if the cell is deterministically
      observed, i.e., when all possible values were observed.  It is
      possible that the coverage is equal [1.0], but [observed] is
      false. For example, if the observer is a random distribution
      observer.
  *)
  val observed : cell -> (bool,'e) m
end

module type T = functor (M : Machine) -> S

type t = (module T)

module Null(Machine : Machine) : S = struct
  open Machine.Syntax
  type ('a,'e) m = ('a,'e) Machine.t constraint 'e = #Context.t
  let zero = Word.zero 8

  type state = {
    addrs : Addr.Set.t;
    vars  : Var.Set.t;
  }

  let empty = {
    addrs = Addr.Set.empty;
    vars = Var.Set.empty;
  }

  let state =
    Machine.Local.create ~name:"null-observer" (fun _ -> empty)

  let update cell t = match cell with
    | Addr a -> {t with addrs = Set.add t.addrs a }
    | Var  v -> {t with vars = Set.add t.vars v  }

  let is_observed cell t = match cell with
    | Addr a -> Set.mem t.addrs a
    | Var  v -> Set.mem t.vars v

  let coverage cell t =
    if is_observed cell t then 1. /. 256. else 0.


  let step cls t = Machine.return ()
  let observe cell =
    Machine.Local.update state (update cell) >>= fun () ->
    Machine.return zero
  let observed _ = Machine.return false
  let coverage cell =
    Machine.Local.get state >>| (coverage cell)

end
