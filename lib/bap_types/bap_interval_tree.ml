open Core_kernel.Std
open Option.Monad_infix

module Seq = Sequence

module type Interval = sig 
  type t [@@deriving compare, sexp_of]
  type point [@@deriving compare, sexp_of]
  val lower : t -> point
  val upper : t -> point
end

module type S = sig
  type 'a t [@@deriving sexp_of]
  type key
  type point

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val least : 'a t -> point option
  val greatest : 'a t -> point option
  val min_binding : 'a t -> (key * 'a) option
  val max_binding : 'a t -> (key * 'a) option
  val add : 'a t -> key -> 'a -> 'a t
  val dominators : 'a t -> key -> (key * 'a) Sequence.t
  val intersections : 'a t -> key -> (key * 'a) Sequence.t
  val intersects : 'a t -> key -> bool
  val dominates : 'a t -> key -> bool
  val contains : 'a t -> point -> bool
  val lookup : 'a t -> point -> (key * 'a) Sequence.t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val remove : 'a t -> key -> 'a t
  val remove_intersections : 'a t -> key -> 'a t
  val remove_dominators : 'a t -> key -> 'a t
  val to_sequence : 'a t -> (key * 'a) Sequence.t
  include Container.S1 with type 'a t := 'a t
end 

module Make(Interval : Interval) = struct
  type key = Interval.t [@@deriving sexp_of]

  module Point = Comparable.Make_plain(struct 
      type t = Interval.point [@@deriving compare, sexp_of]
    end)

  type point = Interval.point [@@deriving compare, sexp_of]


  (* instead of a usual [Empty|Node of _] we use [node option] type to
     represent the tree. This allows us to reuse option monadic
     structure, as well as to evade some allocations when the tree is
     empty and we return [None] (but, I really do not expect that this
     will give any significant speed up).
  *)
  type +'a node = {
    lhs : 'a node option;
    rhs : 'a node option;
    key : key;
    data : 'a;
    height : int;
    greatest : point;
    least : point;
  } [@@deriving fields, sexp_of]

  type +'a t = 'a node option [@@deriving sexp_of]

  let height = Option.value_map ~default:0 ~f:height

  let least = Option.map ~f:least
  let greatest = Option.map ~f:greatest

  let empty = None

  let bound f lhs top rhs = match lhs,rhs with
    | Some x, Some y -> f top (f x y)
    | Some x, None | None, Some x -> f top x
    | None, None -> top

  let create lhs key data rhs =
    let hl,hr = height lhs, height rhs in
    let mn = Interval.lower key in
    let mx = Interval.upper key in
    Some {
      lhs; rhs; key; data;
      height = (if hl >= hr then hl + 1 else hr + 1);
      least = bound Point.min (least lhs) mn (least rhs);
      greatest = bound Point.max (greatest lhs) mx (greatest rhs);
    }

  let singleton key data = create None key data None

  let rec min_binding = function
    | None -> None
    | Some {lhs=None; key; data} -> Some (key,data)
    | Some {lhs} -> min_binding lhs

  let rec max_binding = function
    | None -> None
    | Some {rhs=None; key; data} -> Some (key,data)
    | Some {rhs} -> max_binding rhs

  let bal l x d r =
    let hl,hr = height l, height r in
    if hl > hr + 2 then match l with
      | None -> assert false
      | Some t when height t.lhs >= height t.rhs ->
        create t.lhs t.key t.data (create t.rhs x d r)
      | Some t -> match t.rhs with
        | None -> assert false
        | Some rhs ->
          create (create t.lhs t.key t.data rhs.lhs)
            rhs.key rhs.data (create rhs.rhs x d r)
    else if hr > hl + 2 then match r with
      | None -> assert false
      | Some t when height t.rhs >= height t.lhs ->
        create (create l x d t.lhs) t.key t.data t.rhs
      | Some t -> match t.lhs with
        | None -> assert false
        | Some lhs ->
          create (create l x d lhs.lhs) lhs.key lhs.data
            (create lhs.rhs t.key t.data t.rhs)
    else create l x d r

  let rec add map key data = match map with
    | None -> singleton key data
    | Some t ->
      let c = Interval.compare key t.key in
      if c = 0 then bal map key data None
      else if c < 0
      then bal (add t.lhs key data) t.key t.data t.rhs
      else bal t.lhs t.key t.data (add t.rhs key data)

  let is_inside key p = 
    let low = Interval.lower key and high = Interval.upper key in
    Point.between ~low ~high p

  let lookup start a =
    let open Sequence.Generator in
    let rec go = function
      | None -> return ()
      | Some t when Point.(a < t.least || a > t.greatest) -> return ()
      | Some t ->
        if is_inside t.key a 
        then 
          go t.lhs >>= fun () -> yield (t.key, t.data) >>= fun () -> 
          go t.rhs
        else go t.lhs >>= fun () -> go t.rhs in
    start |> go |> run

  let is_dominated t r =
    let open Interval in
    Point.(lower r >= lower t.key && upper r <= upper t.key)

  let has_intersections t r =
    let open Interval in let open Point in
    let (p,q) = lower t.key, upper t.key
    and (x,y) = lower r, upper r in
    if p <= x
    then x <= q && p <= y
    else p <= y && x <= q

  let can't_be_in_tree t r =
    let open Interval in
    Point.(lower r > t.greatest || Point.(upper r < t.least))

  let can't_be_dominated t r =
    let open Interval in
    Point.(lower r < t.least || upper r > t.greatest)

  let query ~skip_if ~take_if start r =
    let open Sequence.Generator in
    let open Interval in
    let rec go = function
      | None -> return ()
      | Some t when skip_if t r -> return ()
      | Some t when take_if t r ->
        go t.lhs >>= fun () -> yield (t.key, t.data) >>= fun () -> go t.rhs
      | Some t -> go t.lhs >>= fun () -> go t.rhs in
    start |> go |> run

  let dominators m r = query m r
      ~skip_if:can't_be_dominated
      ~take_if:is_dominated

  let intersections m r = query m r
      ~skip_if:can't_be_in_tree
      ~take_if:has_intersections

  let dominates m r = not (Seq.is_empty (dominators m r))
  let intersects m r = not (Seq.is_empty (intersections m r))
  let contains m a = not (Seq.is_empty (lookup m a))

  let rec map m ~f = Option.map m ~f:(fun m -> {
        m with
        lhs = map m.lhs ~f;
        data = f m.data;
        rhs = map m.rhs ~f;
      })

  let rec mapi m ~f = Option.map m ~f:(fun m -> {
        m with
        lhs = mapi m.lhs ~f;
        data = f m.key m.data;
        rhs = mapi m.rhs ~f;
      })


  let rec remove_min_binding = function
    | None -> assert false
    | Some {lhs=None; rhs} -> rhs
    | Some t -> bal (remove_min_binding t.lhs) t.key t.data t.rhs

  let splice t1 t2 =
    match t1,t2 with
    | None, t | t, None -> t
    | _ -> match min_binding t2 with
      | None -> assert false
      | Some (key,data) -> bal t1 key data (remove_min_binding t2)

  let mem_equal x y =
    Interval.(Point.(lower x = lower y) && Point.(upper x = upper y))


  (* This will remove the exact matches.

     Note: since we allow equal bindings in map we should move through
     all sub-tree, but it will be still logarithmic.  *)

  let rec remove map mem = match map with
    | None -> None
    | Some t when mem_equal t.key mem ->
      splice (remove t.lhs mem) (remove t.rhs mem)
    | Some t -> match Interval.compare mem t.key with
      | 1 -> bal t.lhs t.key t.data (remove t.rhs mem)
      | _ -> bal (remove t.lhs mem) t.key t.data t.rhs

  let remove_if ~leave_if ~remove_if map mem =
    let rec remove = function
      | None -> None
      | Some t when leave_if t mem -> Some t
      | Some t when remove_if t mem ->
        splice (remove t.lhs) (remove t.rhs)
      | Some t ->
        bal (remove t.lhs) t.key t.data (remove t.rhs) in
    remove map

  let remove_intersections map mem = remove_if map mem
      ~leave_if:can't_be_in_tree
      ~remove_if:has_intersections

  let remove_dominators map mem = remove_if map mem
      ~leave_if:can't_be_dominated
      ~remove_if:is_dominated

  let filter_mapi map ~f =
    let rec fmap = function
      | None -> None
      | Some t -> match f t.key t.data with
        | None -> splice (fmap t.lhs) (fmap t.rhs)
        | Some data ->
          bal (fmap t.lhs) t.key data (fmap t.rhs) in
    fmap map

  let filter_map map ~f =
    filter_mapi map ~f:(fun _ x -> f x)

  let filter map ~f : _ t =
    filter_map map ~f:(fun x -> Option.some_if (f x) x)

  let to_sequence start =
    let open Seq.Generator in
    let rec go = function
      | None -> return ()
      | Some t ->
        go t.lhs >>= fun () -> yield (t.key, t.data) >>= fun () -> go t.rhs
    in
    start |> go |> run

  (* we do not use include Container.Make(...) per the jane street *)
  (* `core_kernel` `container.ml` best practice documentation *)
  module C = Container.Make(
    struct
      type 'a t = 'a node option
      let rec fold m ~init ~f = Option.fold m ~init:init ~f:(fun acc m ->
          fold m.rhs ~init:(f (fold m.lhs ~init:acc ~f) m.data) ~f)

      let rec iter m ~f = Option.iter m ~f:(fun m ->
          iter m.lhs ~f;
          f m.data;
          iter m.rhs ~f)

      let iter  = `Custom iter
    end)

  let fold = C.fold
  let count = C.count
  let sum = C.sum
  let iter = C.iter
  let length = C.length
  let is_empty = C.is_empty
  let exists = C.exists
  let mem = C.mem
  let for_all = C.for_all
  let find_map = C.find_map
  let find = C.find
  let to_list = C.to_list
  let to_array = C.to_array
  let min_elt = C.min_elt
  let max_elt = C.max_elt
  let fold_until = C.fold_until
  let fold_result = C.fold_result
end

module type Interval_binable = sig
  type t [@@deriving bin_io, compare, sexp]
  type point [@@deriving bin_io, compare, sexp]
  include Interval with type t := t and type point := point
end

module type S_binable = sig
  type 'a t [@@deriving bin_io, compare, sexp]
  include S with type 'a t := 'a t
end

module Make_binable(Interval : Interval_binable) = struct
  module Base = Make(Interval)

  type key = Interval.t [@@deriving sexp, compare, bin_io]

  module Point = Comparable.Make_plain(struct
      type t = Interval.point [@@deriving compare, sexp, bin_io]
    end)

  type point = Interval.point [@@deriving compare, sexp, bin_io]

 type +'a node = 'a Base.node = {
    lhs : 'a node option;
    rhs : 'a node option;
    key : key;
    data : 'a;
    height : int;
    greatest : point;
    least : point;
  } [@@deriving fields, sexp, compare, bin_io]

  type +'a t = 'a node option [@@deriving sexp, compare, bin_io]

  let height = Base.height
  let least = Base.least
  let greatest = Base.greatest
  let empty = Base.empty
  let bound = Base.bound
  let create = Base.create
  let singleton = Base.singleton
  let min_binding = Base.min_binding
  let max_binding = Base.max_binding
  let bal = Base.bal
  let add = Base.add
  let is_inside = Base.is_inside
  let lookup = Base.lookup
  let is_dominated = Base.is_dominated
  let has_intersections = Base.has_intersections
  let can't_be_in_tree = Base.can't_be_in_tree
  let can't_be_dominated = Base.can't_be_dominated
  let query = Base.query
  let dominators = Base.dominators
  let intersections = Base.intersections
  let dominates = Base.dominates
  let intersects = Base.intersects
  let contains = Base.contains
  let map = Base.map
  let mapi = Base.mapi
  let remove_min_binding = Base.remove_min_binding
  let splice = Base.splice
  let mem_equal = Base.mem_equal
  let remove = Base.remove
  let remove_if = Base.remove_if
  let remove_intersections = Base.remove_intersections
  let remove_dominators = Base.remove_dominators
  let filter_mapi = Base.filter_mapi
  let filter_map = Base.filter_map
  let filter = Base.filter
  let to_sequence = Base.to_sequence
  let fold = Base.fold
  let count = Base.count
  let sum = Base.sum
  let iter = Base.iter
  let length = Base.length
  let is_empty = Base.is_empty
  let exists = Base.exists
  let mem = Base.mem
  let for_all = Base.for_all
  let find_map = Base.find_map
  let find = Base.find
  let to_list = Base.to_list
  let to_array = Base.to_array
  let min_elt = Base.min_elt
  let max_elt = Base.max_elt
  let fold_until = Base.fold_until
  let fold_result = Base.fold_result

end
