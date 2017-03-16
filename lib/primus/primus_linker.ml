open Core_kernel.Std
open Bap.Std
open Format
open Primus_types

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



let () = Primus_error.add_printer (function
    | Unbound_name name ->
      Some (asprintf "Linker: unbound %a" Sexp.pp (sexp_of_name name))
    | _ -> None)

let code_of_term sub : code =
  (module functor (_ : Machine) -> struct
    let exec self = self#eval_sub sub
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

let state = Primus_machine.State.declare
    ~uuid:"38bf35bf-1091-4220-bf75-de79db9de4d2"
    ~name:"linker"
    (fun ctxt -> init_state ctxt#program)

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type ('a,'e) m = ('a,'e) Machine.t

  module Biri = Biri.Make(Machine)


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
    | None -> Machine.fail (Unbound_name name)
    | Some (module Code) ->
      let module Code = Code(Machine) in
      Code.exec (biri :> _ Biri.t)

  let is_linked name =
    Machine.Local.get state >>| code_of_name name >>| Option.is_some

end
