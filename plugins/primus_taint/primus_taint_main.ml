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

  let init () =
    Primus.Interpreter.leave_def >>> intro
end

module Pre(Machine : Primus.Machine.S) = struct
  include Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)
  module Tracker = Taint.Tracker(Machine)
  module Object = Taint.Object.Make(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let first_of_kind k taints =
    Machine.Seq.find (Set.to_sequence taints) ~f:(fun t ->
        Object.kind t >>| Taint.Kind.equal k)


  let nil = Value.b0
  let optional value f k x=
    f k x >>= function
    | None -> nil
    | Some x -> value x

  let kind = Kind.to_value
  let taint = Object.to_value
end

module IntroDirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [v; k] =
    Kind.of_value k >>= Tracker.new_direct v >>= taint
end

module IntroIndirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [v; k; n] =
    Kind.of_value k >>=
    Tracker.new_indirect ~addr:v ~len:n >>=
    taint
end

module Has(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run rel [v; k] =
    Kind.of_value k >>= fun k ->
    Tracker.lookup v rel >>=
    optional taint first_of_kind k
end

module HasDirect(Machine : Primus.Machine.S) = struct
  include Has(Machine)
  let run = run Taint.Rel.direct
end

module HasIndirect(Machine : Primus.Machine.S) = struct
  include Has(Machine)
  let run = run Taint.Rel.indirect
end


module Setup(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)

  let init () =
    let open Primus.Lisp.Type.Spec in
    let def name types closure = Lisp.define ~types name closure in
    Machine.sequence [
      def "taint-introduce-directly" (tuple [a; b] @-> c)
        (module IntroDirect);

      def "taint-introduce-indirectly" (tuple [a; int; int] @-> c)
        (module IntroIndirect);

      def "taint-has-direct" (tuple [a; b] @-> c)
        (module HasDirect);

      def "taint-has-indirect" (tuple [a; b] @-> c)
        (module HasIndirect);
    ]
end

let set_default_policy name =
  let module Init(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Policy = Taint.Propagation.Policy.Make(Machine)
    module Value = Primus.Value.Make(Machine)

    let init () =
      Value.Symbol.to_value name >>=
      Policy.of_value >>=
      Policy.set_default
  end in
  Primus.Machine.add_component (module Init)



open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus Taint Analysis Framework. Control Module";
  `P
    "This plugin enables fine tuning of the Taint Analysis Framework, \
     exposes the framework interface to Primus Lisp programs, and
provides several useful components.";
  `S "Taint Propagation Policies";
  `P
    "In the Taint Analysis Framework the taint propagation could be
assigned individually based on a taint kind. If no policy was
assigned, then the default is used (selectable using this module).
It is possible to implement custom policy. This module provides two
policies: $(b,propagate-by-computation) and $(b,propagate-exactly).
The $(b,propagate-by-computation) policy propagates a taint to the
result of any computation. The $(b,propagate-exactly) policy is more
strict and propagate taint if is an operation is store, load, cast, or
concat.";
]

let policy = param string ~default:"propagate-by-computation" "policy"

let () = when_ready (fun {get=(!!)} ->
    Primus.Machine.add_component (module Setup);
    Primus.Machine.add_component (module Intro);
    Primus_taint_policies.init ();
    set_default_policy !!policy)
