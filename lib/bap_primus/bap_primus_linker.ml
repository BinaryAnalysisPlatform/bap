open Core_kernel
open Bap.Std
open Regular.Std
open Format
open Bap_primus_types

type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving bin_io, compare, sexp]

type exn += Unbound_name of name



module type Code = functor (Machine : Machine) -> sig
  val exec : unit Machine.t
end

type code = (module Code)

(* the same piece of code can be referred via multiple names *)
type refs = {
  term : tid option;
  name : string option;
  addr : addr option;
}

type t = {
  codes : code Int.Map.t;
  alias : refs Int.Map.t;
  terms : int Tid.Map.t;
  names : int String.Map.t;
  addrs : int Addr.Map.t;
}

let empty = {
  codes = Int.Map.empty;
  terms = Tid.Map.empty;
  names = String.Map.empty;
  addrs = Addr.Map.empty;
  alias = Int.Map.empty;
}


let unresolved_handler = "__primus_linker_unresolved_call"

include struct
  open Sexp

  let string_of_name = function
    | `symbol name -> name
    | `addr addr -> asprintf "%a" Addr.pp_hex addr
    | `tid tid -> asprintf "%%%a" Tid.pp tid
  let sexp_of_name n = Sexp.Atom (string_of_name n)
  let sexp_of_value {value=x} = Atom (asprintf "%a" Word.pp_hex x)
  let sexp_of_args = List.map ~f:sexp_of_value
  let sexp_of_call (dst,args) =
    List (Atom dst :: sexp_of_args args)
end

let unbound_name,needs_unbound =
  Bap_primus_observation.provide ~inspect:sexp_of_name
    "linker-unbound"

module Trace = struct
  module Observation = Bap_primus_observation
  let exec,will_exec = Observation.provide
      ~inspect:sexp_of_name
      "linker-exec"

  let unresolved,will_fail = Observation.provide
      ~inspect:sexp_of_name
      "linker-unresolved"

  let call,call_entered =
    Observation.provide ~inspect:sexp_of_call "call"

  let return,call_returned =
    Observation.provide ~inspect:sexp_of_call "call-return"
end
include Trace

let () = Exn.add_printer (function
    | Unbound_name name ->
      Some (asprintf "unbound function %s" (string_of_name name))
    | _ -> None)


let add_code code codes =
  let max_key = Option.(Map.max_elt codes >>| fst >>| Int.succ) in
  let key = Option.value ~default:1 max_key in
  key, Map.set codes ~key ~data:code

let id_of_name name s = match name with
  | `symbol name -> Map.find s.names name
  | `addr addr -> Map.find s.addrs addr
  | `tid tid -> Map.find s.terms tid

let code_of_name name s =
  Option.bind (id_of_name name s) ~f:(Map.find s.codes)

let lookup_name k t1 names : string option =
  match Map.find t1 k with
  | None -> None
  | Some idx ->
    Map.to_sequence names |> Seq.find_map ~f:(fun (n,i) ->
        Option.some_if Int.(i = idx) n)

let resolve_name s name = match name with
  | `symbol name -> Some name
  | `addr addr -> lookup_name addr s.addrs s.names
  | `tid tid -> lookup_name tid s.terms s.names

let state = Bap_primus_machine.State.declare
    ~uuid:"38bf35bf-1091-4220-bf75-de79db9de4d2"
    ~name:"linker"
    (fun _ -> empty)

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type 'a m = 'a Machine.t

  let linker_error s = Machine.raise (Unbound_name s)

  let is_linked name =
    Machine.Local.get state >>| code_of_name name >>| Option.is_some

  let do_fail name =
    Machine.Local.get state >>= fun s ->
    match resolve_name s name with
    | Some s -> linker_error (`symbol s)
    | None -> linker_error name

  let run name (module Code : Code) =
    let module Code = Code(Machine) in
    Machine.Observation.make will_exec name >>= fun () ->
    Code.exec

  let fail name =
    Machine.Observation.make will_fail name >>= fun () ->
    Machine.Local.get state >>= fun s ->
    match code_of_name (`symbol unresolved_handler) s with
    | None -> do_fail name
    | Some code -> run name code

  let make_refs term name addr =
    {term; name; addr}

  let link ?addr ?name ?tid code =
    Machine.Local.update state ~f:(fun s ->
        let key,codes = add_code code s.codes in
        let update table value = match value with
          | None -> table
          | Some v -> Map.set table ~key:v ~data:key in
        let terms = update s.terms tid in
        let names = update s.names name in
        let addrs = update s.addrs addr in
        let alias = Map.set s.alias ~key ~data:{
            term=tid;
            name;
            addr
          } in
        {codes; terms; names; addrs; alias})


  let unlink name =
    Machine.Local.get state >>= fun s ->
    match id_of_name name s with
    | None -> Machine.return ()
    | Some id ->
      let cleanup = Map.filter ~f:(fun id' -> id <> id') in
      Machine.Local.put state {
        codes = Map.remove s.codes id;
        alias = Map.remove s.alias id;
        terms = cleanup s.terms;
        names = cleanup s.names;
        addrs = cleanup s.addrs;
      }

  let lookup name =
    Machine.Local.get state >>| code_of_name name

  let exec name =
    lookup name >>= function
    | None -> fail name
    | Some code -> run name code

  let resolve f name =
    Machine.Local.get state >>| fun s ->
    match id_of_name name s with
    | None -> None
    | Some id -> match Map.find s.alias id with
      | None -> None
      | Some refs -> f refs

  let resolve_addr = resolve (fun x -> x.addr)
  let resolve_symbol = resolve (fun x -> x.name)
  let resolve_tid = resolve (fun x -> x.term)

end


module Name = struct
  type t = name
  include Regular.Make(struct
      type t = name [@@deriving bin_io, compare, sexp]
      let hash = Hashtbl.hash
      let version = "1.0.0"
      let module_name = Some "Bap_primus.Std.Linker.Name"
      let pp ppf n =
        Format.fprintf ppf "%s" (string_of_name n)
    end)
end
