open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context

module Make
    (Observer : Primus_observer.T)
    (Machine : Machine) = struct
  open Machine.Syntax

  module Biri = Biri.Make(Machine)
  module Observer = Observer(Machine)

  class ['e] base observer =
    object
      inherit ['e] Biri.t
      constraint 'e = #Context.t

      method! enter_term cls t =
        Machine.get () >>= fun ctxt ->
        match Context.Level.next ctxt#level cls t with
        | Error err -> Machine.fail err
        | Ok next ->
          Machine.put (ctxt#with_level next) >>= fun () ->
          Machine.return ()
    end
end
