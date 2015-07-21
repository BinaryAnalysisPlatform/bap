open Core_kernel.Std
open Bap_types.Std
open Or_error

module Mem = struct
  include Bap_memory
  module T = struct
    type nonrec t = t

    type repr = addr with sexp
    let repr m = min_addr m
    let compare m1 m2 = Addr.compare (repr m1) (repr m2)

    let sexp_of_t t = <:sexp_of<repr>> (repr t)
    let t_of_sexp = opaque_of_sexp
    let hash m = Addr.hash (repr m)
  end
  let sexp_of_t = T.sexp_of_t
  let to_string mem =
    let a1,a2 = min_addr mem, max_addr mem in
    Format.asprintf "[%a - %a]" Addr.pp a1 Addr.pp a2
  include Comparable.Make(T)
  include Hashable.Make(T)
end

module Bound = struct
  type t =
    | Unbound
    | Bounded of addr * addr
  with sexp_of

  let empty = Unbound

  let update bound mem = match bound with
    | Unbound -> Bounded (Mem.min_addr mem, Mem.max_addr mem)
    | Bounded (x,y) ->
      let p = Mem.min_addr mem and q = Mem.max_addr mem in
      Bounded (Addr.min x p, Addr.max y q)

  let is_unbound bound mem = match bound with
    | Unbound -> true
    | Bounded (x,y) ->
      Addr.(Mem.max_addr mem < x) ||
      Addr.(Mem.min_addr mem > y)

  let is_bound bound mem = not (is_unbound bound mem)

  let pp fmt = function
    | Unbound -> Format.fprintf fmt "[]"
    | Bounded (x,y) -> Format.fprintf fmt "[%a,%a]" Addr.pp x Addr.pp y
end

module Map = Mem.Map

type mem = Mem.t with sexp_of

type 'a map = 'a Map.t with sexp_of
type 'a hashable = 'a Hashtbl.Hashable.t

type 'a t = {
  map : 'a map;
  bound : Bound.t;
} with sexp_of

type 'a ranged
  = ?start:mem   (** defaults to the lowest mapped region *)
  -> ?until:mem   (** defaults to the highest mapped area  *)
  -> 'a

let empty = {
  map = Map.empty;
  bound = Bound.empty;
}

let singleton k v = {
  map = Map.singleton k v;
  bound = Bound.(update empty k);
}

let pp_elt f fmt = function
  | None -> Format.fprintf fmt "None"
  | Some (x,_) -> Format.fprintf fmt "Some %a" Addr.pp (f x)

(** @pre [x <= y] *)
let intersects x y = Addr.(Mem.max_addr x >= Mem.min_addr y)

let has_intersections tab (x : mem) : bool =
  Bound.is_bound tab.bound x &&
  match Map.find tab.map x with
  | Some _ -> true
  | None -> match Map.prev_key tab.map x with
    | Some (p,_) when intersects p x -> true
    | _ -> match Map.next_key tab.map x with
      | None -> false
      | Some (n,_) -> intersects x n

(** [left_bound t x ] returns a key to a largest interval in [t] that
    has intersections with [].

    since we can't get the matching key (only lesser or greater ones)
    we will return key [x] itself on a perfect match case. That means,
    that this function shouldn't ever be exposed to a user.

    Otherwise, the only solution (other then just reimplementing our
    own tree) is to sequence all keys and return the head.
*)
let left_bound tab x =
  let rec search_left p = match Map.prev_key tab.map p with
    | Some (p,_) when intersects p x -> search_left p
    | _  -> p in
  if has_intersections tab x then match Map.prev_key tab.map x with
    | Some (p,_) when intersects p x -> Some (search_left p)
    | _ when Map.find tab.map x <> None -> Some x (* see note above*)
    | _ -> Map.next_key tab.map x |> Option.map ~f:fst
  else None

let fold_intersections tab x ~init ~f =
  let rec loop (p,d) init =
    let init = f p d init in
    match Map.next_key tab.map p with
    | Some (n,d) when intersects x n -> loop (n,d) init
    | _ -> init in
  match left_bound tab x with
  | None -> init
  | Some p -> match Map.find tab.map p with
    | Some d -> loop (p,d) init
    | None -> assert false

let intersections tab (x : mem) : 'a seq =
  let open Seq.Generator in
  let init = return () in
  let m = fold_intersections tab x ~init ~f:(fun addr x gen ->
      gen >>= fun () -> yield (addr,x)) in
  run m

let change tab mem ~f =
  let ins = intersections tab mem in
  match f ins with
  | `ignore -> tab
  | `update _ | `rebind _  | `remove as cmd ->
    let map =
      Seq.fold ins ~init:tab.map ~f:(fun m (k,_) -> Map.remove m k) in
    let map, bound = match cmd with
      | `remove -> map, tab.bound
      | `update f ->
        Seq.fold ins ~init:(map, tab.bound)
          ~f:(fun (map,bnd) (mem,x) ->
              Map.add map ~key:mem ~data:(f (mem,x)),
              Bound.update bnd mem)
      | `rebind (mem,data) ->
        Map.add map ~key:mem ~data,
        Bound.update tab.bound mem in
    { map; bound }

let add tab mem x =
  if has_intersections tab mem
  then error "memory has intersections" mem sexp_of_mem
  else Ok {
      map = Mem.Map.add tab.map ~key:mem ~data:x;
      bound = Bound.update tab.bound mem;
    }

let remove tab x =
  (* we shouldn't invalidate our cache if nothing changes *)
  if Map.mem tab.map x
  then {
    tab with
    map = Map.remove tab.map x;
  } else tab

let length tab = Map.length tab.map

let find tab mem = Map.find tab.map mem
let mem  tab mem  = Map.mem tab.map mem

let next tab = Map.next_key tab.map
let prev tab = Map.prev_key tab.map

let min tab = Map.min_elt tab.map
let max tab = Map.max_elt tab.map

let first_some user our =
  Option.first_some user (Option.map ~f:fst our)

let foldi ?start ?until (tab : 'a t) ~(init : 'b) ~f : 'b =
  let f_labeled = fun ~key ~data s -> f key data s in
  if start = None && until = None
  then Map.fold tab.map ~init:init ~f:f_labeled
  else
    let from = first_some start (Map.min_elt tab.map) in
    let last = first_some until (Map.max_elt tab.map) in
    match from,last with
    | Some from, Some last ->
      if Mem.(from <= last)
      then Map.fold_range_inclusive tab.map
          ~init ~min:from ~max:last ~f:f_labeled
      else
        let last,from = from,last in
        let seq =
          Map.to_sequence tab.map
            ~keys_in:(`Decreasing_order_less_than_or_equal_to from) |>
          Seq.take_while ~f:(fun (m,_) -> Mem.(m >= last)) in
        Seq.fold seq ~init ~f:(fun init (addr,x) -> f addr x init)
    | _ -> init

let to_sequence ?start ?until tab =
  let open Seq.Generator in
  let m = foldi ?start ?until tab ~init:(return ())
      ~f:(fun addr v gen -> gen >>= fun () -> yield (addr,v)) in
  run m

let elements ?start ?until tab =
  to_sequence ?start ?until tab |> Seq.map ~f:snd

let regions ?start ?until tab =
  to_sequence ?start ?until tab |> Seq.map ~f:fst

let fold ?start ?until tab ~init ~f =
  foldi tab ~init ~f:(fun _ x s -> f x s)

let iteri ?start ?until (tab : 'a t) ~f : unit =
  foldi ?start ?until tab ~init:() ~f:(fun addr data () -> f addr data)

let iter ?start ?until tab ~f : unit =
  iteri ?start ?until tab ~f:(fun _ data -> f data)

let existsi ?start ?until tab ~f =
  with_return (fun cc ->
      iteri ?start ?until tab ~f:(fun m x -> if f m x then cc.return true);
      false)

let for_alli ?start ?until tab ~f =
  with_return (fun cc ->
      iteri ?start ?until tab ~f:(fun m x -> if not(f m x) then cc.return false);
      true)

let exists ?start ?until tab ~f =
  existsi ?start ?until tab ~f:(fun _ x -> f x)

let for_all ?start ?until tab ~f =
  for_alli ?start ?until tab ~f:(fun _ x -> f x)

let count ?start ?until tab ~f =
  fold ?start ?until ~init:0 tab ~f:(fun x n -> if f x then n + 1 else n)

let find_mapi ?start ?until tab ~f =
  with_return (fun cc ->
      iteri ?start ?until tab ~f:(fun a x -> match f a x with
          | Some x -> cc.return (Some x)
          | None -> ());
      None)

let find_map ?start ?until tab ~f =
  find_mapi ?start ?until tab ~f:(fun _ x -> f x)

let find_if ?start ?until tab ~f =
  find_map ?start ?until tab ~f:(fun x -> if f x then Some x else None)

(** TODO: this very inefficient implementation uses O(N) time, but we
    should provide this, or users will create their own
    implementations. Later, this should be optimized using interval
    tree. *)
let find_addr tab (addr : addr) : (mem * 'a) option =
  with_return (fun cc ->
      iteri tab ~f:(fun mem x -> match Mem.compare_with mem addr with
          | `addr_is_inside -> cc.return (Some (mem,x))
          | `addr_is_below  -> cc.return None
          | `addr_is_above  -> ());
      None)

let make_map map add ?start ?until tab ~f =
  if start = None && until = None
  then {
    bound = tab.bound;
    map = map tab.map ~f:(fun ~key ~data -> f key data);
  } else
    let map,bound =
      foldi ?start ?until tab ~init:(Map.empty,Bound.empty)
        ~f:(fun addr x (map,bound) ->
            add map ~key:addr ~data:(f addr x),
            Bound.update bound addr) in
    { map; bound}

let mapi ?start ?until tab ~f =
  make_map Map.mapi Map.add ?start ?until tab ~f

let map ?start ?until tab ~f =
  mapi ?start ?until tab ~f:(fun _ x -> f x)

let filter_mapi ?start ?until tab ~f =
  let add map ~key ~data = match data with
    | None -> map
    | Some data -> Map.add map ~key ~data in
  make_map Map.filter_mapi add ?start ?until tab ~f

let filter_map ?start ?until tab ~f =
  filter_mapi ?start ?until tab ~f:(fun _ v -> f v)

let filteri ?start ?until tab ~f =
  filter_mapi ?start ?until tab ~f:(fun mem x ->
      if f mem x then Some x else None)

let filter ?start ?until tab ~f =
  filteri ?start ?until tab ~f:(fun _ x -> f x)

let make_mapping t map =
  let size = Map.length map in
  let table =
    Hashtbl.create ~growth_allowed:false ~size ~hashable:t () in
  Map.iter map ~f:(fun ~key:_ ~data:(v1,v2) ->
      Hashtbl.add_exn table ~key:v1 ~data:v2);
  table

let link_maybe_one t (t1 : 'a t) (t2 : 'b t) : 'a -> 'b option =
  let mapping =
    Map.merge t1.map t2.map ~f:(fun ~key -> function
        | `Both (v1,v2) -> Some (v1,v2)
        | `Left  _ | `Right _ -> None) |>
    make_mapping t in
  Hashtbl.find mapping

let sexp_of_hashable t = t.Hashtbl.Hashable.sexp_of_t

let link_one t t1 t2 =
  let map = link_maybe_one t t1 t2 in
  fun x -> match map x with
    | Some y -> y
    | None ->
      let err = Error.create
          "link_exn: unbound value " x (sexp_of_hashable t) in
      Error.raise err

let link_many t t1 t2 =
  let size = Map.length t1.map in
  let table =
    Hashtbl.create ~growth_allowed:false ~size ~hashable:t () in
  Map.iter t1.map ~f:(fun ~key:mem ~data:x ->
      let data = intersections t2 mem |> Seq.map ~f:snd in
      Hashtbl.add_exn table ~key:x ~data);
  fun x -> match Hashtbl.find table x with
    | None -> Seq.empty
    | Some x -> x

let link_at_least_one t t1 t2 =
  let map = link_many t t1 t2 in
  fun x ->
    let y = map x in
    match Seq.hd y, Seq.tl y with
    | Some hd, Some tl -> hd, tl
    | Some hd, None    -> hd, Seq.empty
    | _ ->
      let err = Error.create
          "at_least_one invariant failed.\
           element is not mapped" x (sexp_of_hashable t) in
      Error.raise err

type ('a,_) r =
  | Maybe_one    : ('a, 'a option) r
  | One          : ('a, 'a) r
  | Many         : ('a, 'a seq) r
  | At_least_one : ('a, 'a * 'a seq) r

let maybe_one = Maybe_one
let one = One
let many = Many
let at_least_one = At_least_one

let link : type b c . one_to:((b,c) r) ->
  'a hashable -> 'a t -> b t -> 'a -> c =
  fun ~one_to:r -> match r with
    | One -> link_one
    | Maybe_one -> link_maybe_one
    | At_least_one -> link_at_least_one
    | Many -> link_many

(* reverse injective mapping   *)
let make_rev_map add find t tab =
  let size = Map.length tab.map in
  let table =
    Hashtbl.create ~growth_allowed:false ~hashable:t ~size () in
  Map.iter tab.map ~f:(fun ~key:mem ~data:x ->
      add t table ~key:x ~data:mem);
  find t table

let add_inj t tab ~key ~data =
  match Hashtbl.add tab ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let err = Error.create
        "mapping is non-injective, %s occurs more than once"
        key (sexp_of_hashable t) in
    Error.raise err

let add_sur t tab ~key ~data =
  Hashtbl.add_multi tab ~key ~data

let find_sur t tab key =
  match Hashtbl.find tab key with
  | Some x -> Seq.of_list x
  | None -> Seq.empty

let find_sur_plus t tab key =
  match Hashtbl.find tab key with
  | Some (x::xs) -> x, Seq.of_list xs
  | Some [] | None -> Error.(
      raise @@ create "value %s is unmapped"
        key (sexp_of_hashable t))


let rev_map_exn : type c . (mem,c) r ->
  'a hashable -> 'a t -> 'a -> c = function
  | One -> make_rev_map add_inj (fun _ -> Hashtbl.find_exn)
  | Maybe_one -> make_rev_map add_inj (fun _ -> Hashtbl.find)
  | Many -> make_rev_map add_sur find_sur
  | At_least_one -> make_rev_map add_sur find_sur_plus


let rev_map ~one_to t tab =
  try_with (fun () -> rev_map_exn one_to t tab)
