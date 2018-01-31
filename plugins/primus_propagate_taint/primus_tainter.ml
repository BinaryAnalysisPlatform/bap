open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Bap_strings.Std


module Vid = Primus.Value.Id
type vid = Vid.t

module Object = Primus.Value

type objects = Object.Set.t
type value = Primus.value


module type Value = sig
  type t
  type 'a m
  val to_value : t -> Primus.value m
  val of_value : Primus.value -> t m
end

module Ident(Machine : Primus.Machine.S) = struct
  type t = value
  type 'a m = 'a Machine.t
  let to_value = Machine.return
  let of_value = Machine.return
end

(* state of the taint engine.
 *
 * The taint engine maintains a set of values and pointers that
 * reference tainted objects.
 *
 * Each object is represented by a unique id, that was assigned to it,
 * when the object was originally tainted (i.e., when the taint was introduced).
 *
 * An object could be referenced directly, i.e., it could be a result
 * of a computation (value), represnted by the direct mapping. Or an
 * object could be stored in dymanic memory at the specified address(es).
 *
 * If a tainted object is no longer reachable, then we say that the
 * taint is killed. Thus precise tainting is an instance of the
 * garbage collection problem.
 *
*)
type tainter = {
  direct : objects Vid.Map.t;
  indirect : objects Primus.Value.Map.t;
  taints : value Primus.Value.Map.t;
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
  taints = Object.Map.empty;
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

let taint_killed,killed_taint =
  Primus.Observation.provide
    ~inspect:Primus.sexp_of_value "taint-killed"

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

    include Ident(Machine)
  end
end


module Taint = struct
  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Value = Primus.Value.Make(Machine)

    let next_key taints =
      match Map.min_elt taints with
      | None -> Value.of_int 1 ~width:63
      | Some (k,_) -> Value.succ k

    let gentaint property =
      Machine.Local.get tainter >>= fun s ->
      next_key s.taints >>= fun key ->
      Machine.Local.put tainter {
        s with taints = Map.add s.taints ~key ~data:property
      } >>| fun () -> key

    let direct ~kind ~value =
      gentaint kind >>= fun taint ->
      Machine.Local.update tainter ~f:(fun s -> {
            s with
            direct = Map.update s.direct
                (Primus.Value.id value) ~f:(function
                    | None -> Object.Set.singleton taint
                    | Some taints -> Set.add taints taint)
          })

    let indirectly ~kind ~addr ~len =
      gentaint kind >>= fun taint ->
      Machine.Local.get tainter >>= fun s ->
      Seq.range 0 len |>
      Machine.Seq.fold ~init:s.indirect ~f:(fun indirect off ->
          Value.nsucc addr off >>| fun addr ->
          Map.update indirect addr ~f:(function
              | None -> Object.Set.singleton taint
              | Some taints -> Set.add taints taint)) >>= fun indirect ->
      Machine.Local.put tainter {s with indirect} >>| fun () ->
      taint

    let filter m ~f =
      Map.filter_map m ~f:(fun s ->
          let s = Set.filter s ~f in
          if Set.is_empty s then None else Some s)

    let clear taints kind =
      filter ~f:(fun taint ->
          match Map.find taints taint with
          | None -> assert false
          | Some kind' -> Primus.Value.(kind <> kind'))

    let kind kind =
      Machine.Local.update tainter ~f:(fun s -> {
            s with
            direct = clear s.taints kind s.direct;
            indirect = clear s.taints kind s.indirect;
          })

    let find_taint ~having:kind refs taints key =
      match Map.find refs key with
      | None -> None
      | Some ts -> Set.find ts ~f:(fun t -> match Map.find taints t with
          | None -> assert false
          | Some k -> k = kind)


    let find_all (Rel {field; key}) v =
      Machine.Local.get tainter >>| fun s ->
      match Map.find (Field.get field s) (key v) with
      | None -> Primus.Value.Set.empty
      | Some s -> s

    let find (Rel {field; key}) ~kind v =
      Machine.Local.get tainter >>| fun s ->
      find_taint ~having:kind (Field.get field s) s.taints (key v)

    let sanitize reference ~kind v =
      find reference ~kind v >>= function
      | None -> Machine.return ()
      | Some t ->
        let not_ours t' = Primus.Value.(t <> t') in
        Machine.Local.update tainter ~f:(fun s -> {
              s with
              direct = filter s.direct ~f:not_ours;
              indirect = filter s.indirect ~f:not_ours;
            })


    let union_taints (Rel ms) srcs s =
      List.fold srcs ~init:Object.Set.empty ~f:(fun objs src ->
          match Map.find (Field.get ms.field s) (ms.key src) with
          | None -> objs
          | Some objs' -> Set.union objs' objs)

    let add_taints m key set =
      if Set.is_empty set
      then Map.remove m key
      else Map.add m ~key ~data:set

    (** [ms --> md] transfers references from the [ms] mapping to the
        [md] mapping.  *)
    let transfer ms (Rel md) srcs dst =
      Machine.Local.update tainter ~f:(fun s ->
          union_taints ms srcs s |>
          add_taints (Field.get md.field s) (md.key dst) |>
          Field.fset md.field s)


    (* let attach (Ref {field; get}) ~taint value =
     *   Machine.Local.update tainter ~f:(fun s ->
     *       (Field.get field s)
     *     ) *)


  end
end

module Propagate = struct
  module Computation(Machine : Primus.Machine.S) = struct
    module Eval = Primus.Interpreter.Make(Machine)
    module Taint = Taint.Make(Machine)

    open Machine.Syntax
    let (-->) = Taint.transfer
    let one f (x,y) = f [x] y
    let loaded = one (indirect --> direct)
    let stored = one (direct --> indirect)
    let computed = one (direct --> direct)
    let binop ((_op,x,y),r) = (direct --> direct) [x;y] r
    let unop ((_op,x),r) = computed (x,r)
    let extract ((_,_,x),r) = computed (x,r)
    let cast ((_,_,x),r) = computed (x,r)
    let init () = Machine.sequence Primus.[
        Interpreter.loaded  >>> loaded;
        Interpreter.stored  >>> stored;
        Interpreter.binop   >>> binop;
        Interpreter.unop    >>> unop;
        Interpreter.extract >>> extract;
        Interpreter.cast    >>> cast;
      ]
  end

  type control = {
    path : objects;
  }

  module Control (Machine : Primus.Machine.S) = struct
  end
end

(* tracks live tainted objects, removes unecessary references.

   A tainted object is live, if it is reachable either directly or
   indirectly in the tainted. The indirect references are dropped
   implicitly, when an address is overwritten with the new tainted
   object (since we have finite set of addresses). The direct
   references use value identifiers, that are dynamic, so we need to
   track which of them still live. Since values are only stoted in
   Env, we can easily compute the set of live values.

   Finally, we compute a set of live objects on the entrance to a
   basic block, and a set of live objects at the exit of the basic
   block, the difference between these two are the objects that were
   killed in the block. We make an observation about each killed
   object.

*)
module Gc(Machine : Primus.Machine.S) = struct
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
