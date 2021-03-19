open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_future.Std
open Monads.Std

module Name = Knowledge.Name
module Info = Bap_primus_info

type 'a observation = 'a Univ_map.Key.t
type 'a statement = 'a observation
type 'a t = 'a observation
type ('m,'a) observers = {
  last : int;
  subs : (int * ('a -> 'm)) list
}
type provider = {
  info : Info.t;
  newdata : Sexp.t signal;
  data : Sexp.t stream;
  newtrigger : unit signal;
  triggers : unit stream;
  observers : int;
  key : Sexp.t Univ_map.Key.t;
}

type 'a mstream = {
  providers : provider list;
}

let providers : (string,provider) Hashtbl.t = Hashtbl.create (module String)


let provider ?desc ?(package="primus") name =
  let data,newdata = Stream.create () in
  let triggers,newtrigger = Stream.create () in
  let name = Name.create ~package name in
  let info = Info.create ?desc name in
  let key = Univ_map.Key.create ~name:(Name.show name) ident in
  {info; newdata; data; triggers; newtrigger; observers=0; key}

let update_provider provider ~f =
  Hashtbl.update providers provider ~f:(function
      | None -> failwithf "bug: unregistered provider: %s" provider ()
      | Some p -> f p)

let register_observer =
  update_provider ~f:(fun p -> {p with observers = p.observers + 1})

let provide ?desc ?(inspect=sexp_of_opaque) ?package name =
  let provider = provider ?desc ?package name in
  let name = Name.show @@ Info.name provider.info in
  if Hashtbl.mem providers name then
    failwithf "Observation name `%s' is already used by another \
               component. Please, choose a different name" name ();
  Hashtbl.add_exn providers ~key:name ~data:provider;
  let k = Univ_map.Key.create ~name inspect in
  k,k

let inspect = Univ_map.Key.to_sexp
let name = Univ_map.Key.name
let of_statement = ident

module Key = struct
  type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]
  let to_type_id = ident
end

module Map = Univ_map.Make1(Key)(struct
    type ('a,'m) t = ('a,'m) observers
    let sexp_of_t _ _ = sexp_of_opaque
  end)

type 'e observations = 'e Map.t

type subscription = Subs : _ observation * int -> subscription

let add_observer observers key obs =
  register_observer (name key);
  match Map.find observers key with
  | None ->
    Map.add_exn observers key {last=1; subs=[1,obs]},
    Subs (key,1)
  | Some {last; subs} ->
    Map.set observers key {
      last = last + 1;
      subs = (last + 1, obs) :: subs
    },
    Subs (key,last+1)

let cancel (Subs (key,id)) observers =
  Map.change observers key ~f:(function
      | None -> None
      | Some obs ->
        match List.rev_filter obs.subs ~f:(fun (id',_) -> id <> id') with
        | [] -> None
        | subs -> Some {obs with subs})

let add_watcher observers {key} obs =
  add_observer observers key obs

let callbacks os key =
  match Map.find_exn os key with
  | exception _ -> []
  | {subs} -> subs

module Make(Machine : Monad.S) = struct
  open Machine.Syntax

  let apply inj x = function
    | [] -> Machine.return ()
    | xs ->
      let data = inj x in
      Machine.List.iter xs ~f:(fun (_,ob) -> ob data)

  let push_data p key os ws x =
    let sexp = lazy (inspect key x) in
    if Stream.has_subscribers p.data
    then Signal.send p.newdata (Lazy.force sexp);
    apply ident x os >>= fun () ->
    apply Lazy.force sexp ws

  let notify os key x =
    let p = Hashtbl.find_exn providers (name key) in
    let os = callbacks os key and ws = callbacks os p.key in
    Signal.send p.newtrigger ();
    push_data p key os ws x
  [@@inline]

  let notify_if_observed os key k =
    let p = Hashtbl.find_exn providers (name key) in
    Signal.send p.newtrigger ();
    match callbacks os key, callbacks os p.key with
    | [], [] ->
      if Stream.has_subscribers p.data
      then k @@ fun x ->
        Signal.send p.newdata (inspect key x);
        Machine.return ()
      else Machine.return ()
    | os, ws -> k @@ fun x ->
      push_data p key os ws x
  [@@inline]
end

let empty = Map.empty

let list_providers () = Hashtbl.data providers
let list () = list_providers () |>
              List.map ~f:(fun {info} -> info)

module Provider = struct
  type t = provider
  let name t =
    let name = Info.name t.info in
    let short = Name.unqualified name in
    match Name.package name with
    | "primus" -> short
    | package -> sprintf "%s:%s" package short

  let fullname t = Info.name t.info
  let data t = t.data
  let triggers t = t.triggers
  let observers t = t.observers
end
