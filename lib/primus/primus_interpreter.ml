open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context
module Observation = Primus_observation


let sexp_of_tid t = Sexp.Atom (Tid.name t)

let enter_term, term_entered =
  Observation.provide ~inspect:sexp_of_tid "enter-term"
let leave_term, term_left =
  Observation.provide ~inspect:sexp_of_tid "leave-term"

let sexp_of_level level = Sexp.Atom (Context.Level.to_string level)

let enter_level,level_entered =
  Observation.provide ~inspect:sexp_of_level "enter-level"

let leave_level, level_left =
  Observation.provide ~inspect:sexp_of_level "leave-level"

let sexp_of_term term = sexp_of_tid (Term.tid term)

let enter_sub,sub_entered =
  Observation.provide ~inspect:sexp_of_term "enter-sub"
let enter_arg,arg_entered =
  Observation.provide ~inspect:sexp_of_term "enter-arg"
let enter_blk,blk_entered =
  Observation.provide ~inspect:sexp_of_term "enter-blk"
let enter_phi,phi_entered =
  Observation.provide ~inspect:sexp_of_term "enter-phi"
let enter_def,def_entered =
  Observation.provide ~inspect:sexp_of_term "enter-def"
let enter_jmp,jmp_entered =
  Observation.provide ~inspect:sexp_of_term "enter-jmp"
let enter_top,top_entered =
  Observation.provide ~inspect:sexp_of_term "enter-top"


let leave_sub,sub_left =
  Observation.provide ~inspect:sexp_of_term "leave-sub"
let leave_arg,arg_left =
  Observation.provide ~inspect:sexp_of_term "leave-arg"
let leave_blk,blk_left =
  Observation.provide ~inspect:sexp_of_term "leave-blk"
let leave_phi,phi_left =
  Observation.provide ~inspect:sexp_of_term "leave-phi"
let leave_def,def_left =
  Observation.provide ~inspect:sexp_of_term "leave-def"
let leave_jmp,jmp_left =
  Observation.provide ~inspect:sexp_of_term "leave-jmp"
let leave_top,top_left =
  Observation.provide ~inspect:sexp_of_term "leave-top"

let variable_access,variable_will_be_looked_up =
  Observation.provide ~inspect:sexp_of_var "variable-access"

(* TODO: add sexp_of to the Bil.Result.t *)
let sexp_of_bil_result r = Sexp.Atom (Bil.Result.to_string r)
type bil_result = Bil.Result.t

let variable_read,variable_was_read =
  Observation.provide ~inspect:[%sexp_of: var * bil_result] "variable-read"

let variable_written,variable_was_written =
  Observation.provide ~inspect:[%sexp_of: var * bil_result] "variable-written"

let address_access,address_will_be_read =
  Observation.provide ~inspect:sexp_of_addr "address-access"

let address_read,address_was_read =
  Observation.provide ~inspect:[%sexp_of: addr * word] "address-read"

let address_written,address_was_written =
  Observation.provide ~inspect:[%sexp_of: addr * word]
    "address-written"


module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Expi = Expi.Make(Machine)
  module Biri = Biri.Make(Machine)
  module Memory = Primus_memory.Make(Machine)

  type ('a,'e) state = ('a,'e) Machine.t
  type 'a r = (Bil.result,'a) state
  type 'a u = (unit,'a) Machine.t


  let make_observation = Machine.Observation.make

  class ['e] t  =
    object
      inherit ['e] Biri.t as super
      constraint 'e = #Context.t

      method! enter_term cls t =
        super#enter_term cls t >>= fun () ->
        Machine.get () >>= fun ctxt ->
        make_observation term_entered (Term.tid t) >>= fun () ->
        match Context.Level.next ctxt#level cls t with
        | Error err -> Machine.fail err
        | Ok next ->
          Machine.put (ctxt#with_level next) >>= fun () ->
          make_observation level_left ctxt#level >>= fun () ->
          make_observation level_entered next >>= fun () ->
          Term.switch cls t
            ~program:(make_observation top_entered)
            ~sub:(make_observation sub_entered)
            ~arg:(make_observation arg_entered)
            ~blk:(make_observation blk_entered)
            ~phi:(make_observation phi_entered)
            ~def:(make_observation def_entered)
            ~jmp:(make_observation jmp_entered)

      method! leave_term cls t =
        super#leave_term cls t >>= fun () ->
        make_observation term_left (Term.tid t) >>= fun () ->
        Term.switch cls t
          ~program:(make_observation top_left)
          ~sub:(make_observation sub_left)
          ~arg:(make_observation arg_left)
          ~blk:(make_observation blk_left)
          ~phi:(make_observation phi_left)
          ~def:(make_observation def_left)
          ~jmp:(make_observation jmp_left)

      method! lookup var =
        make_observation variable_will_be_looked_up var >>= fun () ->
        super#lookup var >>= fun r ->
        make_observation variable_was_read (var,r) >>= fun () ->
        Machine.return r

      method! update var r =
        super#update var r >>= fun () ->
        make_observation variable_was_written (var,r)

      method! load _ addr =
        make_observation address_will_be_read addr >>= fun () ->
        Memory.load addr >>= fun r ->
        make_observation address_was_read (addr,r) >>= fun () ->
        super#eval_int r

      method! store mem addr data =
        super#store mem addr data >>= fun r ->
        Memory.store addr data >>= fun () ->
        make_observation address_was_written (addr,data) >>= fun () ->
        Machine.return r

      method! empty : Bil.storage = object(self)
        method load _ = None
        method save _ _ = self
      end

    end
end
