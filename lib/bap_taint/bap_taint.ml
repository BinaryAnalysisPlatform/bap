open Core_kernel
open Bap_primus
open Monads.Std
open Bap_strings.Std
open Bap_knowledge

module Seq = Sequence

module Vid = Primus.Value.Id
type vid = Vid.t

type value = Primus.value
module Machine = Primus.Machine

module type Value = sig
  type t
  type 'a m
  val to_value : t -> Primus.value m
  val of_value : Primus.value -> t m
end

module type comparable_with_value =
  Base.Comparable.S with type
  comparator_witness = Primus.Value.comparator_witness

module Ident = struct
  type t = value
  type 'a m = 'a Primus.machine
  let to_value = Primus.Machine.return
  let of_value = Primus.Machine.return
end

(* registry of all ever created objects *)
type kinds = {
  objects : value Map.M(Primus.Value).t;
}

let kinds = Primus.Machine.State.declare
    ~name:"tainter-object-kinds"
    ~uuid:"a97f9cf6-541a-4127-9eb6-6c24f67ed8b9" @@
  Knowledge.return {objects = Map.empty (module Primus.Value)}

let m63 = Bitvec.modulus 63

module Object = struct
  type Primus.exn += Bad_object of Primus.value

  open Primus.Machine.Syntax
  module Value = Primus.Value


  let next_key {objects} =
    match Map.max_elt objects with
    | None -> Value.one
    | Some (k,_) -> Value.(succ k mod m63)

  let create kind =
    Machine.Local.get kinds >>= fun s ->
    next_key s >>= fun key ->
    Machine.Local.put kinds {
      objects = Map.set s.objects ~key ~data:kind
    } >>| fun () -> key

  let kind v =
    Machine.Local.get kinds >>= fun {objects} ->
    match Map.find objects v with
    | None -> Machine.raise (Bad_object v)
    | Some x -> Machine.return x
  include Ident
  include (Primus.Value : comparable_with_value with type t := value)

  let to_string x =
    Format.asprintf "%a" Bitvec.pp (Primus.Value.to_word x)

  let sexp_of_t x = Sexp.Atom (to_string x)
end

type objects = Set.M(Object).t

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
  direct : objects Map.M(Vid).t;
  indirect : objects Map.M(Primus.Value).t;
} [@@deriving fields]

type relation_kind = Direct | Indirect [@@deriving sexp_of]

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
    kind : relation_kind;
  } -> relation

type gc_local = {
  old : tainter;
}

let empty_tainter = {
  direct = Map.empty (module Vid);
  indirect = Map.empty (module Primus.Value);
}

let tainter = Primus.Machine.State.declare
    ~name:"primus-tainter"
    ~uuid:"2d4a4208-f918-4cf7-8e1b-5d8400a106d3" @@
  Knowledge.return empty_tainter


let gc = Primus.Machine.State.declare
    ~name:"primus-taint-gc"
    ~uuid:"2357826e-d5b7-40a3-8f90-0cfd7b48eadc" @@
  Knowledge.return {old = empty_tainter}

let vid = Primus.Value.id

let indirect = Rel {
    field = Fields_of_tainter.indirect;
    key = ident;
    kind = Indirect;
  }
let direct = Rel {
    field = Fields_of_tainter.direct;
    key = vid;
    kind = Direct;
  }

module Kind = struct
  type t = value
  let create = Primus.Value.Symbol.to_value
  let name = Primus.Value.Symbol.of_value
  include (Primus.Value : comparable_with_value with type t := t)
end

module Rel = struct
  type t = relation
  let direct = direct
  let indirect = indirect
end


module Taint = struct
  type Primus.exn += Bad_cast of Primus.value

  let inspect (Rel {kind},o,v) = Sexp.List [
      sexp_of_relation_kind kind;
      Object.sexp_of_t o;
      Primus.sexp_of_value v;
    ]

  let attached,attach =
    Primus.Observation.provide ~inspect "taint-attached"

  module Value = Primus.Value
  module Taint = Object

  open Machine.Syntax


  let change v (Rel {field; key}) ~f =
    Machine.Local.update tainter ~f:(fun t ->
        Field.fset field t @@
        Map.change (Field.get field t) (key v) ~f)

  let attach v r ts = change v r ~f:(function
      | None -> Some ts
      | Some ts' -> Some (Set.union ts ts')) >>= fun () ->
    Set.to_sequence ts |>
    Machine.Seq.iter ~f:(fun o ->
        Machine.Observation.make attach (r,o,v))


  let detach v r ts = change v r ~f:(function
      | None -> None
      | Some ts' ->
        let ts = Set.diff ts' ts in
        if Set.is_empty ts then None else Some ts)

  let lookup v (Rel {field; key}) =
    Machine.Local.get tainter >>| fun t ->
    match Map.find (Field.get field t) (key v) with
    | None -> Set.empty (module Object)
    | Some s -> s


  let int_of_value v =
    let x = Value.to_word v in
    if Bitvec.fits_int x
    then Machine.return (Bitvec.to_int x)
    else Machine.raise (Bad_cast v)

  let new_direct value kind =
    Taint.create kind >>= fun taint ->
    Machine.Local.update tainter ~f:(fun s -> {
          s with
          direct = Map.update s.direct
              (Primus.Value.id value) ~f:(function
                  | None -> Set.singleton (module Object) taint
                  | Some taints -> Set.add taints taint)
        }) >>| fun () -> taint

  let new_indirect ~addr ~len kind  =
    Taint.create kind >>= fun taint ->
    int_of_value len >>= fun len ->
    Machine.Local.get tainter >>= fun s ->
    Seq.range 0 len |>
    Machine.Seq.fold ~init:s.indirect ~f:(fun indirect off ->
        Value.(nsucc addr off mod m63) >>| fun addr ->
        Map.update indirect addr ~f:(function
            | None -> Set.singleton (module Object) taint
            | Some taints -> Set.add taints taint)) >>= fun indirect ->
    Machine.Local.put tainter {s with indirect} >>| fun () ->
    taint

  exception Bad_object of Primus.value

  let objects_of_kind {objects} k ts =
    Set.filter ts ~f:(fun v ->
        match Map.find objects v with
        | None -> false
        | Some k' -> Kind.(k = k'))

  let sanitize v r k =
    Machine.Local.get kinds >>= fun kinds ->
    change v r ~f:(function
        | None -> None
        | Some ts ->
          let ts = objects_of_kind kinds k ts in
          if Set.is_empty ts then None else Some ts)
end

module Propagation = struct

  type policies = {
    servers : Primus.value Map.M(Kind).t;
    default : Primus.value option;
  }

  let policies = Primus.Machine.State.declare
      ~name:"taint-policies"
      ~uuid:"4c370c33-ef1b-4e10-be27-ee751a4d6cf3" @@
    Knowledge.return {
      servers = Map.empty (module Kind);
      default = None;
    }

  module Policy = struct
    module Tracker = Taint
    module Machine = Primus.Machine
    open Machine.Syntax



    let select p k =
      Machine.Local.update policies ~f:(fun s -> {
            servers = Map.set s.servers ~key:k ~data:p;
            default = Option.first_some s.default (Some p);
          })

    let set_default p = Machine.Local.update policies ~f:(fun s ->
        {s with default = Some p})

    let has_selected ~policy:p ~kind:k {servers; default} =
      match Map.find servers k with
      | Some p' -> Primus.Value.equal p p'
      | None -> match default with
        | None -> false
        | Some p' -> Primus.Value.equal p p'

    let selected policy kind =
      Machine.Local.get policies >>|
      has_selected ~policy ~kind

    let select_objects {objects} policies ~policy ts =
      Set.filter ts ~f:(fun v ->
          match Map.find objects v with
          | None -> false
          | Some kind -> has_selected policies ~policy ~kind)

    let collect_taints p (Rel ms) srcs s =
      Machine.Local.get kinds >>= fun kinds ->
      Machine.Local.get policies >>| fun policies ->
      let filter = select_objects kinds policies ~policy:p in
      let init = Set.empty (module Object) in
      List.fold srcs ~init ~f:(fun objs src ->
          match Map.find (Field.get ms.field s) (ms.key src) with
          | None -> objs
          | Some objs' -> Set.union (filter objs') objs)

    let append_taints m key ts =
      Map.update m key ~f:(function
          | None -> ts
          | Some ts' -> Set.union ts ts')

    let propagate p ms md srcs dst =
      Machine.Local.get tainter >>= fun s ->
      collect_taints p ms srcs s >>= fun taints ->
      Tracker.attach dst md taints
    include Ident
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
  module Machine = Primus.Machine
  open Machine.Syntax
  module Env = Primus.Env
  module Value = Primus.Value

  let sexp_of_finish (t,live) = Sexp.List [
      Primus.sexp_of_value t;
      sexp_of_bool live;
    ]


  let taint_finalize,taint_finished =
    Primus.Observation.provide
      ~inspect:sexp_of_finish "taint-finalize"


  let finish ~live t =
    Machine.Observation.make taint_finished (t,live)

  (* removes directly dead taints *)
  let collect_direct =
    let init = Set.empty (module Vid) in
    Machine.Local.get tainter >>= fun s ->
    Env.all >>= fun vars ->
    Machine.Seq.fold vars ~init ~f:(fun live var ->
        Env.get var >>| fun v ->
        Set.add live (Value.id v)) >>= fun live ->
    Machine.Local.put tainter {
      s with direct = Map.filter_keys s.direct ~f:(Set.mem live)
    }

  (* a set of live tainted objects *)
  let objects {direct; indirect} : objects =
    let init = Set.empty (module Object) in
    let to_set m : objects =
      Map.to_sequence m |>
      Seq.map ~f:snd |>
      Seq.fold ~init ~f:Set.union in
    Set.union (to_set direct) (to_set indirect)

  let main _ =
    Machine.Local.get gc >>= fun {old} ->
    collect_direct >>= fun () ->
    Machine.Local.get tainter >>= fun cur ->
    let dead = Set.diff (objects old) (objects cur) in
    Set.to_sequence dead |> Machine.Seq.iter
      ~f:(finish ~live:false) >>= fun () ->
    Machine.Local.put gc {old = cur}

  let finalize () =
    Machine.Local.get tainter >>= fun cur ->
    Set.to_sequence (objects cur) |>
    Machine.Seq.iter ~f:(finish ~live:true)

  let init () = Machine.sequence [
      Primus.Machine.effect >>> main;
      Primus.Machine.finished >>> finalize;
    ]

end


module Std = struct
  module type Value = Value
  module Taint = struct
    module Object = Object
    module Kind = Kind
    module Rel = Rel
    module Tracker = Taint
    module Propagation = Propagation
    module Gc = Gc
    let attached = Taint.attached
  end
end
