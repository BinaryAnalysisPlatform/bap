open Core_kernel.Std
open Bap.Std
open Format
open Bap_primus_types

type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving sexp_of]

type error += Unbound_name of name

module type Code = functor (Machine : Machine) -> sig
  val exec : (#Context.t as 'a) Biri.Make(Machine).t -> (unit,'a) Machine.t
end


type code = (module Code)

type t = {
  codes : code Int.Map.t;
  terms : int Tid.Map.t;
  names : int String.Map.t;
  addrs : int Addr.Map.t;
}

let empty = {
  codes = Int.Map.empty;
  terms = Tid.Map.empty;
  names = String.Map.empty;
  addrs = Addr.Map.empty;
}


let string_of_name = function
  | `symbol name -> name
  | `addr addr -> sprintf "at address %s" (Addr.string_of_value addr)
  | `tid tid -> sprintf "with tid %a" Tid.pps tid

let () = Bap_primus_error.add_printer (function
    | Unbound_name name ->
      Some (asprintf "unbound function %s" (string_of_name name))
    | _ -> None)

let code_of_term sub : code =
  (module functor (Machine : Machine) -> struct
    let exec :
      (#Context.t as 'a) Biri.Make(Machine).t -> (unit,'a) Machine.t =
      fun self -> self#eval sub_t sub
  end)

let add_code code codes =
  let max_key = Option.(Map.max_elt codes >>| fst >>| Int.succ) in
  let key = Option.value ~default:1 max_key in
  key, Map.add codes ~key ~data:code

let link_code t s =
  let key,codes = add_code (code_of_term t) s.codes in
  {
    codes;
    terms = Map.add s.terms ~key:(Term.tid t) ~data:key;
    names = Map.add s.names ~key:(Sub.name t) ~data:key;
    addrs = match Term.get_attr t address with
      | None -> s.addrs
      | Some addr -> Map.add s.addrs ~key:addr ~data:key
  }

let init_state program =
  (object
    inherit [t] Term.visitor
    method! enter_sub = link_code
  end)#run program empty

let find k1 m1 m2 = match Map.find m1 k1 with
  | None -> None
  | Some k2 -> Map.find m2 k2

let code_of_name name s = match name with
  | `symbol name -> find name s.names s.codes
  | `addr addr -> find addr s.addrs s.codes
  | `tid tid -> find tid s.terms s.codes

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
    (fun ctxt -> init_state ctxt#program)

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type ('a,'e) m = ('a,'e) Machine.t

  module Biri = Biri.Make(Machine)

  let linker_error s = Machine.fail (Unbound_name s)

  let fail name =
    Machine.Local.get state >>= fun s ->
    match resolve_name s name with
    | Some s -> linker_error (`symbol s)
    | None -> linker_error name

  let link ?addr ?name ?tid code  =
    Machine.Local.update state ~f:(fun s ->
        let key,codes = add_code code s.codes in
        let update table value = match value with
          | None -> table
          | Some v -> Map.add table ~key:v ~data:key in
        let terms = update s.terms tid in
        let names = update s.names name in
        let addrs = update s.addrs addr in
        {codes; terms; names; addrs})

  let exec name biri =
    Machine.Local.get state >>| code_of_name name >>= function
    | None -> fail name
    | Some (module Code) ->
      let module Code = Code(Machine) in
      Code.exec (biri :> _ Biri.t)

  let is_linked name =
    Machine.Local.get state >>| code_of_name name >>| Option.is_some

end
