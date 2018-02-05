open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
module Legacy_taint = Taint
open Bap_taint.Std
open Format
include Self()


module Intro(Machine : Primus.Machine.S) = struct
  module Env = Primus.Interpreter.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Tracker = Taint.Tracker(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Object = Taint.Object.Make(Machine)

  open Machine.Syntax


  let kind t =
    Kind.create (sprintf "user-%s" (Tid.name (Term.tid t)))

  let gentaint t =
    kind t >>= fun k ->
    Object.create k >>|
    Taint.Object.Set.singleton

  let introduces def =
    Term.has_attr def Legacy_taint.reg ||
    Term.has_attr def Legacy_taint.ptr

  let taint_ptr taints ptr sz =
    Eval.exp ptr >>= fun ptr ->
    Machine.Seq.iter (Seq.range 0 (Size.in_bytes sz)) ~f:(fun off ->
        Value.nsucc ptr off >>= fun ptr ->
        Tracker.attach ptr Taint.Rel.indirect taints)

  let taint_var taints var =
    Env.get var >>= fun v ->
    Tracker.attach v Taint.Rel.direct taints

  let intro def =
    if introduces def then
      gentaint def >>= fun t ->
      taint_var t (Def.lhs def) >>= fun () ->
      match Def.rhs def with
      | Bil.Load (_,addr,_,sz)
      | Bil.Store (_,addr,_,_,sz) ->
        taint_ptr t addr sz
      | exp ->
        Exp.free_vars exp |> Set.to_sequence |>
        Machine.Seq.iter ~f:(taint_var t)
    else Machine.return ()
end

module Pre(Machine : Primus.Machine.S) = struct
  include Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)
  module Tracker = Taint.Tracker(Machine)
  module Object = Taint.Object.Make(Machine)
  module Kind = Taint.Kind.Make(Machine)
end

module IntroDirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [v; k] = Kind.of_value k >>= Tracker.new_direct v
end

module IntroIndirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [v; k; n] =
    Kind.of_value k >>=
    Tracker.new_indirect ~addr:v ~len:n
end

module HasDirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [v; k] =
    Tracker.lookup v Taint.Rel.direct
end

(* module IntroIndirect(Machine : Primus.Machine.S) = struct
 *   include Pre(Machine)
 *   [@@@warning "-P"]
 *
 *   let general
 *
 * end *)



open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus Taint Analysis Framework.";
]





let () = when_ready (fun _ -> ())
