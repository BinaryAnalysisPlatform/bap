open Core_kernel.Std
open Bap.Std
open Bap_future.Std

type 'a observation = 'a Univ_map.Key.t
type 'a statement = 'a observation
type 'a t = 'a observation
type ('m,'a) observers = Observers of ('a -> 'm) list
type provider = {
  name : string;
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

let providers : provider String.Table.t = String.Table.create ()

let provider name =
  let data,newdata = Stream.create () in
  let triggers,newtrigger = Stream.create () in
  let key = Univ_map.Key.create ~name:("watch-"^name) ident in
  {name; newdata; data; triggers; newtrigger; observers=0; key}

let update_provider provider ~f =
  Hashtbl.update providers provider ~f:(function
      | None -> failwith "bug: unregistered provider"
      | Some p -> f p)

let register_observer =
  update_provider ~f:(fun p -> {p with observers = p.observers + 1})

let provide ?(inspect=sexp_of_opaque) name =
  if Hashtbl.mem providers name then
    failwithf "Observation name `%s' is already used by another \
               component. Please, choose a different name" name ();
  Hashtbl.add_exn providers ~key:name ~data:(provider name);
  let k = Univ_map.Key.create ~name inspect in
  k,k

let inspect = Univ_map.Key.to_sexp
let name = Univ_map.Key.name
let of_statement = ident

module Map = Univ_map.Make1(struct
    type ('a,'m) t = ('a,'m) observers
    let sexp_of_t _ _ = sexp_of_opaque
  end)

type 'e observations = 'e Map.t

let add_observer observers key obs =
  register_observer (name key);
  Map.update observers key ~f:(function
      | None -> Observers [obs]
      | Some (Observers observers) -> Observers (obs::observers))

let add_watcher observers {key} obs =
  add_observer observers key obs

let notify_provider key x =
  let p = Hashtbl.find_exn providers (name key) in
  if Stream.has_subscribers p.data
  then Signal.send p.newdata (inspect key x);
  Signal.send p.newtrigger ()

let notify_observer os key x =
  match Map.find os key with
  | None -> Seq.empty
  | Some (Observers os) -> Seq.(map ~f:(fun ob -> ob x) @@ of_list os)

let notify_watchers os key x =
  let p = Hashtbl.find_exn providers (name key) in
  match Map.find os p.key with
  | None -> Seq.empty
  | Some (Observers os) ->
    let data = (inspect key x) in
    Seq.(map ~f:(fun ob -> ob data) @@ of_list os)

let notify os key x =
  notify_provider key x;
  Seq.append
    (notify_observer os key x)
    (notify_watchers os key x)


let empty = Map.empty

let list_providers () = Hashtbl.data providers

module Provider = struct
  type t = provider
  let name t = t.name
  let data t = t.data
  let triggers t = t.triggers
  let observers t = t.observers
end
