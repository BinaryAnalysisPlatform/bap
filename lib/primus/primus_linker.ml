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
  codes : code Tid.Map.t;
  names : tid String.Map.t;
  addrs : tid Addr.Map.t;
}

let empty = {
  codes = Tid.Map.empty;
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

let link_code t s =
  let tid = Term.tid t in
  { s with
    codes = Map.add s.codes ~key:tid ~data:(code_of_term t);
    addrs = match Term.get_attr t address with
      | None -> s.addrs
      | Some addr -> Map.add s.addrs ~key:addr ~data:tid
  }

let init_state program =
  (object
    inherit [t] Term.visitor
    method! enter_sub t s =
      let s = link_code t s in
      {s with
       names = Map.add s.names ~key:(Sub.name t) ~data:(Term.tid t)}
  end)#run program empty

let find k1 m1 m2 = match Map.find m1 k1 with
  | None -> None
  | Some k2 -> Map.find m2 k2


let find_by_tid {names} tid =
  Map.to_sequence names |> Seq.find_map ~f:(fun (name,tid') ->
      if Tid.equal tid tid' then Some name else None)

let find_by_addr s addr  =
  Map.to_sequence s.addrs |>
  Seq.find ~f:(fun (a,tid) -> Addr.equal addr a) |> function
  | Some (_,tid) -> find_by_tid s tid
  | None -> None

  let code_of_name name s = match name with
    | `symbol name -> find name s.names s.codes
    | `addr addr -> find addr s.addrs s.codes
    | `tid tid -> Map.find s.codes tid

let state = Primus_machine.State.declare
    ~uuid:"38bf35bf-1091-4220-bf75-de79db9de4d2"
    ~name:"linker"
    (fun ctxt -> init_state ctxt#program)

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type ('a,'e) m = ('a,'e) Machine.t

  module Biri = Biri.Make(Machine)


  let link ?addr ?name ~code tid =
    Machine.Local.update state ~f:(fun s ->
        let codes = Map.add s.codes ~key:tid ~data:code in
        let names = match name with
          | None -> s.names
          | Some name -> Map.add s.names ~key:name ~data:tid in
        let addrs = match addr with
          | None -> s.addrs
          | Some addr -> Map.add s.addrs  ~key:addr ~data:tid in
        {codes; names; addrs})

  let exec name biri =
    Machine.Local.get state >>| code_of_name name >>= function
    | None -> Machine.fail (Unbound_name name)
    | Some (module Code) ->
      let module Code = Code(Machine) in
      Code.exec (biri :> _ Biri.t)

  let is_linked name =
    Machine.Local.get state >>| code_of_name name >>| Option.is_some

  let resolve name =
    Machine.Local.get state >>| fun c -> match name with
    | `symbol name -> Some name
    | `addr addr -> find_by_addr c addr
    | `tid  tid  -> find_by_tid c tid
end
