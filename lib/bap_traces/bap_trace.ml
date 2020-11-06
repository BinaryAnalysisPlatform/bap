open Core_kernel
open Regular.Std
open Result
open Bap.Std

module Id = Bap_trace_id
module Tab = String.Caseless.Table
module Unix = Caml_unix

type event = value [@@deriving bin_io, sexp, compare]

type step = [
  | `Stop
  | `Skip
  | `Fail
  | `Make of event
] [@@deriving variants]

type users_handler = [ `User of (event Or_error.t seq -> event seq) ]
type proto = string
type tool  = string [@@deriving bin_io, sexp]
type id = Id.t [@@deriving bin_io, compare, sexp]

module Monitor = struct
  type t = (Error.t -> step)
  let fail_on_error e = Error.raise e
  let ignore_errors _ = skip
  let stop_on_error _ = stop
  let warn_on_error f e = f e; skip
  let pack_errors f e = `Make (f e)
  let create f        = f
end

type monitor = Monitor.t

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

type +'a result = ('a,error) Result.t

module Reader = struct
  type t = {
    tool : tool;
    meta : dict;
    next : unit -> event Or_error.t option;
  }
end

open Reader

type reader = Reader.t

type t = {
  id     : id;
  proto  : proto option;
  reader : reader;
  mapper : event -> event option;
  monitor: Error.t -> step
}

let mk_tab = Tab.create
let tools  : (module S) Tab.t = mk_tab ()
let protos : (module P) Tab.t = mk_tab ()
let readers: (Uri.t -> id -> (reader, io_error) Result.t) Tab.t = mk_tab ()
let writers: (Uri.t -> t -> (unit, io_error) Result.t) Tab.t = mk_tab ()

let make_id () = Bap_trace_id.create `V4

let make_error (#error as r) = Error r

let protocols_of_uri uri =
  Hashtbl.fold protos ~init:[]
    ~f:(fun ~key ~data acc ->
        let module A = (val data : P) in
        if A.probe uri then key::acc
        else acc)

let find_proto uri : 'a result =
  match protocols_of_uri uri with
  | [] -> make_error `No_provider
  | p::[] -> Ok p
  | _ -> make_error `Ambiguous_uri

let find_by_proto tab proto : 'a result =
  match Hashtbl.find tab proto with
  | None -> make_error `No_provider
  | Some x -> Ok x

let of_reader id proto monitor reader =
  {id; reader; monitor; proto; mapper = Option.some}

let load ?(monitor=Monitor.fail_on_error) uri =
  find_proto uri >>= fun proto ->
  find_by_proto readers proto >>= fun create_reader ->
  let id = make_id () in
  match create_reader uri id with
  | Ok reader -> Ok (of_reader id (Some proto) monitor reader)
  | Error err -> Error (err :> error)

let save uri t : 'a result =
  find_proto uri >>= fun proto ->
  find_by_proto writers proto >>= fun write ->
  match write uri t with
  | Ok () -> Ok ()
  | Error e -> Error (e :> error)

let create ?(monitor=Monitor.fail_on_error) tool next =
  let reader = {
    tool;
    meta = Dict.empty;
    next;
  } in
  of_reader (make_id ()) None monitor reader

let set_attr t attr v =
  let set t = {
    t with
    meta = match Dict.add t.meta attr v with
      | `Ok meta -> meta
      | `Duplicate ->
        Dict.change t.meta attr (fun _ -> (Some v))
  } in
  {t with reader = set t.reader}

let id t = t.id
let tool t = t.reader.tool
let meta t = t.reader.meta
let get_attr t = Dict.find t.reader.meta
let has_attr t = Dict.mem  t.reader.meta
let set_meta t meta =
  {t with reader = {t.reader with meta}}

let supports_by_tool t tag =
  let t = Hashtbl.find_exn tools t.reader.tool in
  let module Tool = (val t : S) in
  Tool.supports tag

let supports_by_proto t tag = match t.proto with
  | None -> true
  | Some p ->
    let p = Hashtbl.find_exn protos p in
    let module Proto = (val p : P) in
    Proto.supports tag

let supports t tag = supports_by_tool t tag && supports_by_proto t tag

let read_events t : event seq =
  let map e = match t.mapper e with
    | None -> Seq.Step.Skip ()
    | Some e -> Seq.Step.Yield (e,()) in
  Seq.unfold_step ~init:() ~f:(fun () ->
      match t.reader.next () with
      | None -> Seq.Step.Done
      | Some (Ok e) -> map e
      | Some Error e -> match t.monitor e with
        | `Stop -> Seq.Step.Done
        | `Fail -> Error.raise e
        | `Skip -> Seq.Step.Skip ()
        | `Make e -> Seq.Step.Yield (e,()))

let read_all t tag =
  read_events t |> Seq.filter_map ~f:(Value.get tag)

let read_all_matching t matcher =
  read_events t |> Seq.map ~f:(Value.Match.select matcher)

let next_event t = read_events t |> Seq.hd
let next t tag = read_all t tag |> Seq.hd
let next_matching t m = read_all_matching t m |> Seq.hd

let filter_map t ~f = {t with mapper = fun ev ->
    match t.mapper ev with
    | None -> None
    | Some ev -> f ev
  }

let add tab key data = Hashtbl.add_exn tab ~key ~data
let register_reader proto init = add readers proto init
let register_writer proto write = add writers proto write

let register_tool : (module S) -> tool = fun s ->
  let module A = (val s : S) in
  add tools A.name s; A.name

let register_proto : (module P) -> proto = fun p ->
  let module A = (val p : P) in
  add protos A.name p; A.name
