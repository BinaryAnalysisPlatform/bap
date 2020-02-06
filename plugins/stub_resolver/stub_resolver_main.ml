open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
include Self ()

let is_section name v =
  match Value.get Image.section v with
  | Some x -> String.(x = name)
  | _ -> false

let eval slot exp =
  let cls = Knowledge.Slot.cls slot in
  match Knowledge.run cls exp (Toplevel.current ()) with
  | Ok (v,_) -> Ok (Knowledge.Value.get slot v)
  | Error conflict -> Error conflict

let find_aliases (tid : tid) =
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


module Plt_resolver = struct

  let memory_contains m sub =
    match Term.get_attr sub address with
    | None -> false
    | Some addr -> Set.mem m addr

  (* pre: all functions have different names
     pre: there aren't any stubs with the same alias *)
  let partition mem prog =
    let add_aliases stubs stub =
      let tid = Term.tid stub in
      Set.fold ~init:stubs (find_aliases tid)
        ~f:(fun stubs alias -> Map.set stubs alias tid) in
    let add_sym syms s = Map.set syms (Sub.name s) (Term.tid s) in
    let names = Map.empty (module String) in
    Term.to_sequence sub_t prog |>
    Seq.fold ~init:(names,names) ~f:(fun (stubs, syms) sub ->
        if memory_contains mem sub
        then add_aliases stubs sub, syms
        else stubs, add_sym syms sub)

  let find_unambiguous_pairs syms stubs =
    Map.fold stubs ~init:(Map.empty (module Tid))
      ~f:(fun ~key:name ~data:tid pairs ->
          match Map.find syms name with
          | None -> pairs
          | Some tid' -> Map.add_multi pairs tid tid') |>
    Map.filter_map ~f:(function
        | [tid] -> Some tid
        | _ -> None)

  let run proj =
    let mem = section_memory proj ".plt" in
    if Set.is_empty mem
    then Map.empty (module Tid)
    else
      let stubs, subs = partition mem (Project.program proj) in
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

let run proj =
  let links = Plt_resolver.run proj in
  if Map.is_empty links then proj
  else
    Project.with_program proj (relink (Project.program proj) links)

let () =
  let _man = Config.manpage [
      `S "DESCRIPTION";

      `P "This plugin provides an abi pass that transforms a program
          by substituting calls to stubs with calls to real
          subroutines.";

      `P "Thus, for stubs in .plt section, the correspondence is
          based on symbols names: if a stub from plt section has the
          same name as any other subroutine, then all calls to this stub
          will be replaced with calls to the subroutine."

    ] in
  Config.when_ready (fun _ -> Bap_abi.register_pass run)
