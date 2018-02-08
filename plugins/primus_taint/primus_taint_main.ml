open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Bap_taint.Std
open Format
include Self()

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
  let run [k; v] =
    Kind.of_value k >>= Tracker.new_direct v >>= taint
end

module IntroIndirect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [k; v; n] =
    Kind.of_value k >>=
    Tracker.new_indirect ~addr:v ~len:n >>=
    taint
end

module Get(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run rel [k; v] =
    Kind.of_value k >>= fun k ->
    Tracker.lookup v rel >>=
    optional taint first_of_kind k
end

module GetDirect(Machine : Primus.Machine.S) = struct
  include Get(Machine)
  let run = run Taint.Rel.direct
end

module GetIndirect(Machine : Primus.Machine.S) = struct
  include Get(Machine)
  let run = run Taint.Rel.indirect
end

module Sanitize(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run rel [k; v] =
    Kind.of_value k >>= fun k ->
    Tracker.sanitize v rel k >>| fun () ->
    v
end

module SanitizeDirect(Machine : Primus.Machine.S) = struct
  include Sanitize(Machine)
  let run = run Taint.Rel.direct
end

module SanitizeIndirect(Machine : Primus.Machine.S) = struct
  include Sanitize(Machine)
  let run = run Taint.Rel.indirect
end

module TaintKind(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [t] =
    Object.of_value t >>=
    Object.kind >>=
    Kind.to_value
end


module PolicySelect(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  module Policy = Taint.Propagation.Policy.Make(Machine)
  let run [k; p] =
    Kind.of_value k >>= fun k ->
    Policy.of_value p >>= fun p ->
    Policy.select k p >>= fun () ->
    nil
end

module PolicySetDefault(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  module Policy = Taint.Propagation.Policy.Make(Machine)
  let run [p] =
    Policy.of_value p >>=
    Policy.set_default >>= fun () ->
    nil
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

      def "taint-get-direct" (tuple [a; b] @-> c)
        (module GetDirect);

      def "taint-get-indirect" (tuple [a; b] @-> c)
        (module GetIndirect);

      def "taint-sanitize-direct" (tuple [a; b] @-> c)
        (module SanitizeDirect);

      def "taint-sanitize-indirect" (tuple [a; b] @-> c)
        (module SanitizeIndirect);

      def "taint-policy-select" (tuple [a; b] @-> bool)
        (module PolicySelect);

      def "taint-policy-set-default" (tuple [a] @-> bool)
        (module PolicySetDefault);

      def "taint-kind" (tuple [a] @-> b)
        (module TaintKind);

    ];
end

module Signals(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  module Object = Taint.Object.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let init () = Machine.sequence [
      Lisp.signal Taint.Gc.taint_finalize @@ fun (t,live) ->
      Machine.List.all [
        Object.to_value t;
        Value.of_bool live;
      ]
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

  `P "The Primus Taint Analysis Framework Control Module";

  `P "This plugin enables fine tuning of the Taint Analysis Framework,
    exposes the framework interface to Primus Lisp programs, and
    provides several useful components.";

  `S "Taint Propagation Policies";

  `P "In the Taint Analysis Framework the taint propagation could be
    assigned individually based on a taint kind. If no policy was
    assigned, then the default is used (selectable using this module).
    It is possible to implement custom policy. This module provides
    two policies: $(b,propagate-by-computation) (default) and
    $(b,propagate-exactly).  The $(b,propagate-by-computation) policy
    propagates a taint to the result of any computation. The
    $(b,propagate-exactly) policy is more strict and propagate taint
    if is an operation is store, load, cast, or concat.";

  `S "Taint Garbage Collectors";

  `P "It is possible to track the liveness of a taint, i.e., whether
    it is reachable from any live value. By default we use a
    conservative garbage collector that sometimes gives a taint more
    credit than it deserves, in other words a taint may be treated as
    live even if it is no longer reachable. When a taint is destroyed,
    either because it is not longer reachable or because a machine is
    finished, a taint finalizer, that is reflected to the
    $(b,taint-finalize), Lisp signal is called, with the taint itself,
    and a boolean parameter that tells whether the taint was still
    live or dead, when the finalization was called."
]

let policy = param string ~default:"propagate-by-computation" "policy"
    ~doc:"Set the default taint propagation policy"

(* our poor choice of garbage collectors *)
let collectors = [
  "none", false;
  "conservative", true;
]

let enable_gc = param (enum collectors) ~default:true "gc"
    ~doc:"Picks a taint garbage collector"

let () = when_ready (fun {get=(!!)} ->
    Primus.Machine.add_component (module Setup);
    Primus.Machine.add_component (module Signals);
    Primus_taint_policies.init ();
    set_default_policy !!policy;
    if !!enable_gc
    then Primus.Machine.add_component (module Taint.Gc.Conservative))
