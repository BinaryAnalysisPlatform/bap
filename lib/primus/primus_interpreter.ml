open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context

module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Biri = Biri.Make(Machine)

  class ['e] base = object
    inherit ['e] Biri.t
    constraint 'e = #Context.t

    method! enter_term cls t =
      Machine.get () >>= fun ctxt ->
      match Context.Level.next ctxt#level cls t with
      | Ok next -> Machine.put (ctxt#with_level next)
      | Error err -> Machine.fail err
  end
end
