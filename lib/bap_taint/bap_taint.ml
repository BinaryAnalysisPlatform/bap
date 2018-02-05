open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Bap_strings.Std


module Vid = Primus.Value.Id
type vid = Vid.t

type value = Primus.value


module type Value = sig
  type t
  type 'a m
  val to_value : t -> Primus.value m
  val of_value : Primus.value -> t m
end

module type comparable_with_value =
  Comparable.S_plain with type comparator_witness = Primus.Value.comparator_witness

module Ident(Machine : Primus.Machine.S) = struct
  type t = value
  type 'a m = 'a Machine.t
  let to_value = Machine.return
  let of_value = Machine.return
end

(* registry of all ever created objects *)
type kinds = {
  objects : value Primus.Value.Map.t;
}

let kinds = Primus.Machine.State.declare
    ~name:"tainter-object-kinds"
    ~uuid:"a97f9cf6-541a-4127-9eb6-6c24f67ed8b9"
    (fun _ -> {objects = Primus.Value.Map.empty})

module Object = struct
  module Make(Machine : Primus.Machine.S) = struct
    module Value = Primus.Value.Make(Machine)

    open Machine.Syntax

    let next_key {objects} =
      match Map.min_elt objects with
      | None -> Value.of_int 1 ~width:63
      | Some (k,_) -> Value.succ k

    let create kind =
      Machine.Local.get kinds >>= fun s ->
      next_key s >>= fun key ->
      Machine.Local.put kinds {
        objects = Map.add s.objects ~key ~data:kind
      } >>| fun () -> key

    (* pre: v was obtained from (create k)
       post: returns k.

       we can break the precondition only inside of this module, where
       the abstraction walls are not yet erected, and the equality
       between object and value types is not removed. *)
    let kind v =
      Machine.Local.get kinds >>| fun {objects} ->
      Map.find_exn objects v
    include Ident(Machine)
  end
  include (Primus.Value : comparable_with_value with type t = value)
end

type objects = Object.Set.t

(* state of the taint engine.
 *
 * The taint engine maintains a set of values and pointers that
 * reference tainted objects. Another interpretation is that we are
 * attaching some semantic information (represented as identifiers) to
 * immediate _and_ addresses.
 *
 * Each object is represented by a unique id that was assigned to it,
 * when the object was originally tainted (i.e., when the taint was introduced).
 *
 * So far we have only two kinds of relations between a value and a
 * tainted value - direct, when a value either contains bits of the
 * tainted object or is influenced by them, and indirect, when a value
 * is an address to a tainted value. We may later add more relations,
 * for example, when a value is not influenced by the object that we
 * track, but in fact contains the object (or bits of this
 * object). So far we are keeping the relation object abstract that
 * will allow us to extend it later.
 *
 * If a tainted object is no longer reachable, then we say that the
 * taint is killed. Thus precise tainting is an instance of the
 * garbage collection problem.
*)
type tainter = {
  direct : objects Vid.Map.t;
  indirect : objects Primus.Value.Map.t;
} [@@deriving fields]

(* generalized access to the tainter fields
 *
 * - [select.field] selects either direct or indirect field of the
 *   tainter (should be used with [Field.get] and [Field.fset])
 *  - [select.key] maps value to the key of the appropriate type
 *   (i.e., to Id in case of direct, and addr in case of indirect)
 *
 *  With the selector object it is possible to write a function that
 *  will be polymorphic across two fields of the tainter (sort of
 *  extremly boundned polymorphism).
*)
type relation = Rel : {
    field : (tainter, ('k,objects,'c) Map.t) Field.t;
    key : (Primus.value -> 'k);
  } -> relation

type gc_local = {
  old : tainter;
}

let empty_tainter = {
  direct = Vid.Map.empty;
  indirect = Primus.Value.Map.empty;
}

let tainter = Primus.Machine.State.declare
    ~name:"primus-tainter"
    ~uuid:"2d4a4208-f918-4cf7-8e1b-5d8400a106d3"
    (fun _ -> empty_tainter)


let gc = Primus.Machine.State.declare
    ~name:"primus-taint-gc"
    ~uuid:"2357826e-d5b7-40a3-8f90-0cfd7b48eadc"
    (fun _ -> {old = empty_tainter})

let taint_introduced,introduced_taint =
  Primus.Observation.provide
    ~inspect:Primus.sexp_of_value "taint-introduced"

let taint_propagated,propagated_taint =
  Primus.Observation.provide
    ~inspect:Primus.sexp_of_value "taint-propagated"


let vid = Primus.Value.id

let indirect = Rel {
    field = Fields_of_tainter.indirect;
    key = ident;
  }
let direct = Rel {
    field = Fields_of_tainter.direct;
    key = vid;
  }

module Kind = struct
  type t = value
  module Make(Machine : Primus.Machine.S) = struct
    module Value = Primus.Value.Make(Machine)
    let create = Value.Symbol.to_value
    let name = Value.Symbol.of_value
    include Ident(Machine)
  end
  include (Primus.Value : comparable_with_value with type t := t)
end

module Rel = struct
  type t = relation
  let direct = direct
  let indirect = indirect
end



module Taint = struct
  type Primus.exn += Bad_cast of Primus.value

  module Make(Machine : Primus.Machine.S) = struct
    module Value = Primus.Value.Make(Machine)
    module Taint = Object.Make(Machine)

    open Machine.Syntax


    let change v (Rel {field; key}) ~f =
      Machine.Local.update tainter ~f:(fun t ->
          Field.fset field t @@
          Map.change (Field.get field t) (key v) ~f)

    let attach v r ts = change v r ~f:(function
        | None -> Some ts
        | Some ts' -> Some (Set.union ts ts'))

    let detach v r ts = change v r ~f:(function
        | None -> None
        | Some ts' ->
          let ts = Set.diff ts' ts in
          if Set.is_empty ts then None else Some ts)

    let lookup v (Rel {field; key}) =
      Machine.Local.get tainter >>| fun t ->
      match Map.find (Field.get field t) (key v) with
      | None -> Object.Set.empty
      | Some s -> s


    let int_of_value x =
      match Word.to_int (Value.to_word x) with
      | Error _ -> Machine.raise (Bad_cast x)
      | Ok x -> Machine.return x

    let new_direct value kind =
      Taint.create kind >>= fun taint ->
      Machine.Local.update tainter ~f:(fun s -> {
            s with
            direct = Map.update s.direct
                (Primus.Value.id value) ~f:(function
                    | None -> Object.Set.singleton taint
                    | Some taints -> Set.add taints taint)
          }) >>| fun () -> taint

    let new_indirect ~addr ~len kind  =
      Taint.create kind >>= fun taint ->
      int_of_value len >>= fun len ->
      Machine.Local.get tainter >>= fun s ->
      Seq.range 0 len |>
      Machine.Seq.fold ~init:s.indirect ~f:(fun indirect off ->
          Value.nsucc addr off >>| fun addr ->
          Map.update indirect addr ~f:(function
              | None -> Object.Set.singleton taint
              | Some taints -> Set.add taints taint)) >>= fun indirect ->
      Machine.Local.put tainter {s with indirect} >>| fun () ->
      taint

    exception Bad_object of Primus.value

    let objects_of_kind {objects} k ts =
      Set.filter ts ~f:(fun v ->
          match Map.find objects v with
          | None -> false
          | Some k' -> Kind.(k <> k'))

    let sanitize v r k =
      Machine.Local.get kinds >>= fun kinds ->
      change v r ~f:(function
          | None -> None
          | Some ts ->
            let ts = objects_of_kind kinds k ts in
            if Set.is_empty ts then None else Some ts)


    let collect_taints kind (Rel ms) srcs s =
      Machine.Local.get kinds >>| fun kinds ->
      let filter = objects_of_kind kinds kind in
      List.fold srcs ~init:Object.Set.empty ~f:(fun objs src ->
          match Map.find (Field.get ms.field s) (ms.key src) with
          | None -> objs
          | Some objs' -> Set.union (filter objs') objs)

    let append_taints m key ts =
      Map.update m key ~f:(function
          | None -> ts
          | Some ts' -> Set.union ts ts')

    let transfer kind ms md srcs dst =
      Machine.Local.get tainter >>= fun s ->
      collect_taints kind ms srcs s >>= fun taints ->
      attach dst md taints
  end
end

module Propagation = struct

  type policies = {
    clients : Primus.Value.Set.t Primus.Value.Map.t;
    default : Primus.Value.t option;
  }

  let policies = Primus.Machine.State.declare
      ~name:"taint-policies"
      ~uuid:"4c370c33-ef1b-4e10-be27-ee751a4d6cf3"
      (fun _ -> {
           clients = Primus.Value.Map.empty;
           default = None;
         })

  module Policy = struct
    type t = value
    module Make(Machine : Primus.Machine.S) = struct
      open Machine.Syntax

      let select k p =
        Machine.Local.update policies ~f:(fun s -> {
              clients = Map.update s.clients p ~f:(function
                  | None -> Primus.Value.Set.singleton k
                  | Some ks -> Set.add ks k);
              default = Option.first_some s.default (Some p);
            })

      let set_default p = Machine.Local.update policies ~f:(fun s ->
          {s with default = Some p})

      let kinds p : Kind.Set.t Machine.t =
        Machine.Local.get policies >>| fun {clients} ->
        match Map.find clients p with
        | None -> Primus.Value.Set.empty
        | Some ks -> ks


      include Ident(Machine)
    end
  end


  module Support(Id : sig val name : string end)
      (Machine : Primus.Machine.S) = struct
    module Policy = Policy.Make(Machine)
    module Taint = Taint.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    open Machine.Syntax

    let name = Value.Symbol.to_value Id.name

    let (-->) rs rd srcs dst =
      name >>= Policy.kinds >>= fun ks ->
      Set.to_sequence ks |> Machine.Seq.iter ~f:(fun k ->
          Taint.transfer k rs rd srcs dst)
  end

  module Computation(Machine : Primus.Machine.S) = struct
    module Id = struct
      let name = "propagate-by-computation"
    end
    module Eval = Primus.Interpreter.Make(Machine)
    module Taint = Taint.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Support = Support(Id)(Machine)
    open Machine.Syntax
    open Support


    let one f (x,y) = f [x] y

    let loaded = one (indirect --> direct)
    let computed = one (direct --> direct)
    let concat ((x,y),r) = (direct --> direct) [x;y] r
    let binop ((_op,x,y),r) = (direct --> direct) [x;y] r
    let unop ((_op,x),r) = computed (x,r)
    let extract ((_,_,x),r) = computed (x,r)
    let cast ((_,_,x),r) = computed (x,r)

    let stored (x,y) =
      Taint.lookup y indirect >>= fun ts ->
      Taint.detach y indirect ts >>= fun () ->
      (direct --> indirect) [x] y

    let init () = Machine.sequence Primus.[
        Interpreter.loaded  >>> loaded;
        Interpreter.stored  >>> stored;
        Interpreter.binop   >>> binop ;
        Interpreter.unop    >>> unop;
        Interpreter.extract >>> extract;
        Interpreter.concat  >>> concat;
        Interpreter.cast    >>> cast;
      ]
  end

  module Exact(Machine : Primus.Machine.S) = struct
    module Id = struct
      let name = "propagate-exactly"
    end
    module Eval = Primus.Interpreter.Make(Machine)
    module Taint = Taint.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Support = Support(Id)(Machine)

    open Machine.Syntax
    open Support

    let name = Value.Symbol.to_value

    let one f (x,y) = f [x] y

    let loaded = one (indirect --> direct)
    let computed = one (direct --> direct)
    let concat ((x,y),r) = (direct --> direct) [x;y] r
    let extract ((_,_,x),r) = computed (x,r)
    let cast ((_,_,x),r) = computed (x,r)

    let stored (x,y) =
      Taint.lookup y indirect >>= fun ts ->
      Taint.detach y indirect ts >>= fun () ->
      (direct --> indirect) [x] y

    let init () = Machine.sequence Primus.[
        Interpreter.loaded  >>> loaded;
        Interpreter.stored  >>> stored;
        Interpreter.extract >>> extract;
        Interpreter.concat  >>> concat;
        Interpreter.cast    >>> cast;
      ]
  end
end

(* tracks live tainted objects, removes unecessary references.

   A tainted object is live, if it is reachable either directly or
   indirectly in the tainted. The indirect references are dropped
   implicitly, when an address is overwritten with the new tainted
   object (since we have finite set of addresses). The direct
   references use value identifiers, that are dynamic, so we need to
   track which of them still live. Since values are stored only in
   Env, we can easily compute the set of live values.

   Finally, we compute a set of live objects on the entrance to a
   basic block, and a set of live objects at the exit of the basic
   block, the difference between these two are the objects that were
   killed in the block. We make an observation about each killed
   object.

*)
module Gc = struct

  let taint_killed,killed_taint =
    Primus.Observation.provide
      ~inspect:Primus.sexp_of_value "taint-killed"

  module Conservative(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Env = Primus.Env.Make(Machine)
    module Value = Primus.Value.Make(Machine)

    (* removes directly dead taints *)
    let collect_direct =
      Machine.Local.get tainter >>= fun s ->
      Env.all >>= fun vars ->
      Machine.Seq.fold vars ~init:Vid.Set.empty ~f:(fun live var ->
          Env.get var >>| fun v ->
          Set.add live (Value.id v)) >>= fun live ->
      Machine.Local.put tainter {
        s with direct = Map.filter_keys s.direct ~f:(Set.mem live)
      }

    (* a set of live tainted objects *)
    let objects {direct; indirect} : objects =
      let to_set m : objects =
        Map.to_sequence m |>
        Seq.map ~f:snd |>
        Seq.fold ~init:Object.Set.empty ~f:Set.union in
      Set.union (to_set direct) (to_set indirect)

    let main _ =
      Machine.Local.get gc >>= fun {old} ->
      collect_direct >>= fun () ->
      Machine.Local.get tainter >>= fun cur ->
      let dead = Set.diff (objects old) (objects cur) in
      Set.to_sequence dead |> Machine.Seq.iter
        ~f:(Machine.Observation.make killed_taint) >>= fun () ->
      Machine.Local.put gc {old = cur}


    let init () = Primus.Interpreter.leave_blk >>> main

  end
end


module Std = struct
  module type Value = Value
  module Taint = struct
    module Object = Object
    module Kind = Kind
    module Rel = Rel
    module Tracker = Taint.Make
    module Propagation = Propagation
    module Gc = Gc
  end
end
