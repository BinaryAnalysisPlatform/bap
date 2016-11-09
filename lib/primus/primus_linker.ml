open Core_kernel.Std
open Bap.Std
open Primus_types

type error += Unbound_name of name

module type S = Linker

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type ('a,'e) m = ('a,'e) Machine.t
  type 'e context = 'e constraint 'e = #Context.t
  type code = { exec : 'e. unit -> (unit,'e context) m}

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

  let code_of_sub sub = {
    exec = fun () ->
      Machine.update @@ fun ctxt -> ctxt#set_next (Some (Term.tid sub))
  }

  let init_state program =
    Term.enum sub_t program |> Seq.fold ~init:empty ~f:(fun s sub ->
        let tid = Term.tid sub in
        {
          codes = Map.add s.codes ~key:tid ~data:(code_of_sub sub);
          names = Map.add s.names ~key:(Sub.name sub) ~data:tid;
          addrs = match Term.get_attr sub address with
            | None -> s.addrs
            | Some addr -> Map.add s.addrs ~key:addr ~data:tid
        })

  let state = Machine.Local.create ~name:"linker" (fun ctxt ->
      init_state ctxt#program)


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

  let find k1 m1 m2 = match Map.find m1 k1 with
    | None -> None
    | Some k2 -> Map.find m2 k2

  let code_of_name name s = match name with
    | `symbol name -> find name s.names s.codes
    | `addr addr -> find addr s.addrs s.codes
    | `tid tid -> Map.find s.codes tid

  let exec name =
    Machine.Local.get state >>| code_of_name name >>= function
    | None -> Machine.fail (Unbound_name name)
    | Some code -> code.exec ()

  let is_linked name =
    Machine.Local.get state >>| code_of_name name >>| Option.is_some


  let find_by_tid {names} tid =
    Map.to_sequence names |> Seq.find_map ~f:(fun (name,tid') ->
        if Tid.equal tid tid' then Some name else None)

  let find_by_addr s addr  =
    Map.to_sequence s.addrs |>
    Seq.find ~f:(fun (a,tid) -> Addr.equal addr a) |> function
    | Some (_,tid) -> find_by_tid s tid
    | None -> None

  let resolve name =
    Machine.Local.get state >>| fun c -> match name with
    | `symbol name -> Some name
    | `addr addr -> find_by_addr c addr
    | `tid  tid  -> find_by_tid c tid
end
