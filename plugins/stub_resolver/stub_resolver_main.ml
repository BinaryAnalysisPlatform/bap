open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
include Self ()

module type Resolver = sig
  val run : Project.t -> tid Tid.Map.t
end

module Resolver(R : sig
    val is_stub : sub term -> bool
    val is_stub_of : sub term -> of_:sub term -> bool
  end) = struct

  let partition prog =
    let stubs = Seq.fold Term.to_sequence

end




module Plt_resolver = struct

  let is_section name v =
    match Value.get Image.section v with
    | Some x -> String.(x = name)
    | _ -> false

  let eval slot exp =
    let cls = Knowledge.Slot.cls slot in
    match Knowledge.run cls exp (Toplevel.current ()) with
    | Ok (v,_) -> Ok (Knowledge.Value.get slot v)
    | Error conflict -> Error conflict

  let collect_names (tid : tid) =
    match eval Theory.Label.aliases (Knowledge.return tid) with
    | Ok names -> names
    | Error _ -> Set.empty (module String)

  let section_memory proj sec_name =
    let collect_addresses addrs (mem,_ )=
      Memory.foldi mem ~word_size:`r8 ~init:addrs
        ~f:(fun addr _ acc -> Set.add acc addr) in
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.fold ~init:(Set.empty (module Addr)) ~f:collect_addresses

  let is_a_plt_stub mem sub =
    match Term.get_attr sub address with
    | None -> false
    | Some addr -> Set.mem mem addr

  (* pre: all functions have different names *)
  let partition plt prog =
    let add_name tid subs name = Map.set subs name tid in
    let add_stub stubs stub =
      let tid = Term.tid stub in
      Set.fold ~init:stubs (collect_names tid) ~f:(add_name tid) in
    let add_sub subs sub =
      add_name (Term.tid sub) subs (Sub.name sub) in
    Term.to_sequence sub_t prog |>
    Seq.fold ~init:(Map.empty (module String), Map.empty (module String))
      ~f:(fun (stubs, subs) sub ->
          if is_a_plt_stub plt sub
          then add_stub stubs sub, subs
          else stubs, add_sub subs sub)

  let find_unambiguous_pairs subs stubs =
    let pairs, duplicates =
      Map.fold stubs ~init:(Map.empty (module Tid), Set.empty (module Tid))
        ~f:(fun ~key:name ~data:tid (pairs, duplicates) ->
            match Map.find subs name with
            | None -> pairs, duplicates
            | Some tid' ->
              match Map.add pairs tid tid' with
              | `Ok pairs  -> pairs, duplicates
              | `Duplicate -> pairs, Set.add duplicates tid) in
    Set.fold duplicates ~init:pairs ~f:Map.remove

  let run proj =
    let plt = section_memory proj ".plt" in
    if Set.is_empty plt
    then Map.empty (module Tid)
    else
      let stubs, subs = partition plt (Project.program proj) in
      find_unambiguous_pairs subs stubs

end

let relink prog links =
  (object
    inherit Term.mapper

    method! map_jmp jmp =
      match Jmp.kind jmp with
      | Goto _ | Int _ | Ret _ -> jmp
      | Call c ->
        match Call.target c with
        | Indirect _ -> jmp
        | Direct tid ->
          match Map.find links tid with
          | None -> jmp
          | Some tid' ->
            Jmp.with_kind jmp (Call (Call.with_target c (Direct tid')))
  end)#run prog

let relink resolver proj =
  let module R = (val resolver : Resolver) in
  let links = R.run proj in
  if Map.is_empty links then proj
  else
    Project.with_program proj (relink (Project.program proj) links)

let run proj = relink (module Plt_resolver) proj


let () =
  let _man = Config.manpage [
      `S "DESCRIPTION";

      `P "This plugin provides an abi pass that transforms a program
          in the following way: for any stub symbol (e.g. a plt entry)
          it tries to find a symbol with the same name and if a symbol
          was found, it substitues any call to a stub symbol with a
          call to the found one."
    ] in
  Config.when_ready (fun _ -> Bap_abi.register_pass run)
