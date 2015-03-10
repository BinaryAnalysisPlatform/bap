open Core_kernel.Std
open Bap_types.Std
open Option.Monad_infix

module Mem = struct
  include Bap_memory
  let compare x y =
    Addr.compare (min_addr x) (min_addr y)
end
type mem = Mem.t with sexp_of


(* instead of a usual [Empty|Node of _] we use [node option] type to
   represent the tree. This allows us to reuse option monadic
   structure, as well as to evade some allocations when the tree is
   empty and we return [None] (but, I really do not expect that this
   will give any significant speed up).
*)
type +'a node = {
  lhs : 'a node option;
  rhs : 'a node option;
  key : mem;
  data : 'a;
  height : int;
  max_addr : addr;
  min_addr : addr;
} with fields, sexp_of

type +'a t = 'a node option with sexp_of

let height = Option.value_map ~default:0 ~f:height

let min_addr = Option.map ~f:min_addr
let max_addr = Option.map ~f:max_addr

let empty = None

let bound f lhs top rhs = match lhs,rhs with
  | Some x, Some y -> f top (f x y)
  | Some x, None | None, Some x -> f top x
  | None, None -> top

let create lhs key data rhs =
  let hl,hr = height lhs, height rhs in
  let mn = Mem.min_addr key in
  let mx = Mem.max_addr key in
  Some {
    lhs; rhs; key; data;
    height = (if hl >= hr then hl + 1 else hr + 1);
    min_addr = bound Addr.min (min_addr lhs) mn (min_addr rhs);
    max_addr = bound Addr.max (max_addr lhs) mx (max_addr rhs);
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
    let c = Mem.compare key t.key in
    if c = 0 then bal map key data None
    else if c < 0
    then bal (add t.lhs key data) t.key t.data t.rhs
    else bal t.lhs t.key t.data (add t.rhs key data)

let lookup start a =
  let open Sequence.Generator in
  let rec go = function
    | None -> return ()
    | Some t when Addr.(a < t.min_addr || a > t.max_addr) -> return ()
    | Some t ->
      match Mem.compare_with t.key a with
      | `addr_is_inside ->
        go t.lhs >>= fun () -> yield (t.key, t.data) >>= fun () -> go t.rhs
      | _ -> go t.lhs >>= fun () -> go t.rhs in
  start |> go |> run

let is_dominated t r =
  let open Mem in
  Addr.(min_addr r >= min_addr t.key && max_addr r <= max_addr t.key)

let has_intersections t r =
  let open Mem in let open Addr in
  let (p,q) = min_addr t.key, max_addr t.key
  and (x,y) = min_addr r, max_addr r in
  if p <= x
  then x <= q && p <= y
  else p <= y && x <= q

let can't_be_in_tree t r =
  let open Mem in
  Addr.(min_addr r > t.max_addr || Addr.(max_addr r < t.min_addr))

let can't_be_dominated t r =
  let open Mem in
  Addr.(min_addr r < t.min_addr || max_addr r > t.max_addr)

let query ~skip_if ~take_if start r =
  let open Sequence.Generator in
  let open Mem in
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
  Mem.(Addr.(min_addr x = min_addr y) && Int.(length x = length y))


(* This will remove the exact matches.

   Note: since we allow equal bindings in map we should move through
   all sub-tree, but it will be still logarithmic.  *)

let rec remove map mem = match map with
  | None -> None
  | Some t when mem_equal t.key mem ->
    splice (remove t.lhs mem) (remove t.rhs mem)
  | Some t -> match Mem.compare mem t.key with
    | 1 -> bal t.lhs t.key t.data (remove t.rhs mem)
    | _ -> bal (remove t.lhs mem) t.key t.data t.rhs

let remove_if ~leave_if ~remove_if map (mem : mem) =
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
