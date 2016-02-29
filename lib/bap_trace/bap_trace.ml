open Core_kernel.Std
open Regular.Std
open Result
open Bap.Std

module Id = Bap_trace_id
module Tab = String.Caseless.Table

type event = value with bin_io, sexp, compare

type error_handler = [
  | `Fail
  | `Miss
  | `Stop
  | `Pack of (Error.t -> event)
  | `Warn of (Error.t -> unit)
]

type users_handler = [ `User of (event Or_error.t seq -> event seq) ]
type monitor = [ error_handler | users_handler ]
type proto = string
type tool  = string with bin_io, sexp
type id = Id.t with bin_io, compare, sexp

module Monitor = struct
  type t = monitor
  let fail_on_error   = `Fail
  let ignore_errors   = `Miss
  let stop_on_error   = `Stop
  let warn_on_error f = `Warn f
  let pack_errors f   = `Pack f
  let create f        = `User f
end

module type S = sig
  val name: string
  val supports: 'a tag -> bool
end

module type P = sig
  include S
  val probe: Uri.t -> bool
end

type io_error = [
  | `Protocol_error of Error.t  (** Data encoding problem         *)
  | `System_error of Unix.error (** System error                  *)
]

type error = [
  | io_error
  | `No_provider    (** No provider for a given URI               *)
  | `Ambiguous_uri  (** More than one provider for a given URI    *)
]

module Reader = struct
  type t = {
    tool : tool;
    meta : dict;
    next : unit -> event Or_error.t option;
  }
end

type reader = Reader.t

type t = {
  id     : id;
  meta   : dict;
  events : event seq;
  tool   : tool;
  proto  : proto option;
}

let mk_tab = Tab.create
let tools  : (module S) Tab.t = mk_tab ()
let protos : (module P) Tab.t = mk_tab ()
let readers: (Uri.t -> id -> (reader, io_error) Result.t) Tab.t = mk_tab ()
let writers: (Uri.t -> t -> (unit, io_error) Result.t) Tab.t = mk_tab ()

let make_id () = Bap_trace_id.create `V4

let make_error = function
  | #error as r -> Error r

let protocols_of_uri uri =
  Hashtbl.fold protos ~init:[]
    ~f:(fun ~key ~data acc ->
        let module A = (val data : P) in
        if A.probe uri then key::acc
        else acc)

let find_proto uri  =
  match protocols_of_uri uri with
  | [] -> make_error `No_provider
  | p::[] -> Ok p
  | protos -> make_error `Ambiguous_uri

let find_by_proto tab proto =
  try
    Ok (Hashtbl.find_exn tab proto)
  with Not_found -> make_error `No_provider

let make_stream next init monitor =
  let open Seq.Step in
  let if_error m =
    fun a -> match next a with
      | None -> Done
      | Some (Ok ev, a') -> Yield (ev, a')
      | Some (Error err, a') ->
        match m with
        | `Fail -> Error.raise err
        | `Miss -> Skip a'
        | `Pack f -> Yield (f err, a')
        | `Stop -> Done
        | `Warn f -> f err; Skip a' in
  let ident a = match next a with
    | None -> Done
    | Some (elt, a') -> Yield (elt, a') in
  let s = match monitor with
    | #error_handler as m -> Seq.unfold_step ~init ~f:(if_error m)
    | `User handle -> Seq.unfold_step ~init ~f:ident |> handle in
  Seq.memoize s

let wrap_next f =
  fun () -> match f () with
    | None -> None
    | Some elt -> Some (elt, ())

let of_reader id proto monitor reader =
  let tool = reader.Reader.tool in
  let meta = reader.Reader.meta in
  let next = wrap_next reader.Reader.next in
  let events = make_stream next () monitor in
  {id; meta; tool; events; proto = Some proto;}

let map_result f = function
  | Ok v -> Ok (f v)
  | Error err -> make_error err

let load ?(monitor=`Fail) uri =
  find_proto uri >>= fun proto ->
  find_by_proto readers proto >>= fun make ->
  let id = make_id () in
  make uri id |>
  map_result (of_reader id proto monitor)

let save uri t =
  find_proto uri >>= fun proto ->
  find_by_proto writers proto >>= fun write ->
  write uri t |>
  map_result ident

let create tool = {
  id = make_id ();
  meta = Dict.empty;
  events = Seq.empty;
  tool;
  proto = None;
}

let unfold ?(monitor=`Fail) tool ~f ~init =
  let id = make_id () in
  let meta = Dict.empty in
  let proto = None in
  let events = make_stream f init monitor in
  {id; meta; tool; events; proto;}

let unfold' ?(monitor=`Fail) tool ~f =
  unfold ~monitor tool ~f:(wrap_next f) ~init:()

let set_attr t attr v =
  let meta = match Dict.add t.meta attr v with
    | `Ok meta -> meta
    | `Duplicate -> Dict.change t.meta attr (fun _ -> (Some v)) in
  {t with meta}

let id t = t.id
let tool t = t.tool
let meta t = t.meta
let get_attr t = Dict.find t.meta
let has_attr t = Dict.mem  t.meta
let set_meta t meta = {t with meta}
let events t = t.events
let find t tag = Seq.find_map t.events ~f:(Value.get tag)
let find_all t tag = Seq.filter_map t.events ~f:(Value.get tag)

let supports_by_tool t tag =
  let t = Hashtbl.find_exn tools t.tool in
  let module Tool = (val t : S) in
  Tool.supports tag

let supports_by_proto t tag = match t.proto with
  | None -> true
  | Some p ->
    let p = Hashtbl.find_exn protos p in
    let module Proto = (val p : P) in
    Proto.supports tag

let supports t tag = supports_by_tool t tag && supports_by_proto t tag
let memoize t = { t with events = Seq.force_eagerly t.events }

let fold_matching t matcher ~f ~init =
  let f' acc ev = f acc (Value.Match.switch ev matcher) in
  Seq.fold t.events ~init ~f:f'

let find_all_matching t matcher =
  Seq.map ~f:(Value.Match.select matcher) t.events

let contains t tag =
  let in_trace = Seq.exists t.events ~f:(Value.is tag) in
  if in_trace || supports t tag then Some in_trace
  else None

let add_event t tag e =
  let ev = Value.create tag e in
  let events = Seq.append t.events (Seq.singleton ev) in
  {t with events}

let append t evs = {t with events = Seq.append t.events evs}
let add tab key data = Hashtbl.add_exn tab ~key ~data
let register_reader proto init = add readers proto init
let register_writer proto write = add writers proto write

let register_tool : (module S) -> tool = fun s ->
  let module A = (val s : S) in
  add tools A.name s; A.name

let register_proto : (module P) -> proto = fun p ->
  let module A = (val p : P) in
  add protos A.name p; A.name
