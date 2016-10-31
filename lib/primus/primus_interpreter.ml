open Core_kernel.Std
open Bap.Std

open Primus_types

module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Biri = Biri.Make(Machine)

  class ['e] base = object
    inherit ['e] Biri.t
    constraint 'e = #Primus_context.t

    (* method! enter_term cls t = *)
    (*   Machine.get () >>= fun ctxt -> *)
    (*   match next_level cls t with *)
    (*   | None -> assert false *)
    (*   | Some level -> Machine.put (ctxt#with_level level) *)
  end
end
