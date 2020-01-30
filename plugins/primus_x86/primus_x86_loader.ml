open Core_kernel
open Bap.Std
open Bap_primus.Std
open X86_cpu
open Bap_core_theory
open Bap_knowledge

let is_section name v =
  match Value.get Image.section v with
  | Some x -> String.(x = name)
  | _ -> false

let eval slot exp =
  let cls = Knowledge.Slot.cls slot in
  match Knowledge.run cls exp (Toplevel.current ()) with
  | Ok (v,s) -> Ok (Knowledge.Value.get slot v)
  | Error conflict -> Error conflict

let collect_names (tid : tid) =
  match eval Theory.Label.aliases (Knowledge.return tid) with
  | Ok names -> names
  | Error _ -> Set.empty (module String)


module Relink_plt = struct

  let section_memory proj sec_name =
    let collect_addresses addrs (mem,_ )=
      Memory.foldi mem ~word_size:`r8 ~init:addrs
        ~f:(fun addr _ acc -> Set.add acc addr) in
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.fold ~init:(Set.empty (module Addr))
      ~f:collect_addresses

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

  let main proj =
    let plt = section_memory proj ".plt" in
    let stubs, subs = partition plt (Project.program proj) in
    let pairs = find_unambiguous_pairs subs stubs in
    Project.with_program proj (relink (Project.program proj) pairs)
end

module Plt_jumps(Machine : Primus.Machine.S) = struct
  module Linker = Primus.Linker.Make(Machine)
  open Machine.Syntax

  let section_memory sec_name =
    Machine.get () >>| fun proj ->
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.map ~f:fst

  let load_table =
    Machine.arch >>= fun arch ->
    section_memory ".got.plt" >>| fun memory ->
    Seq.fold memory ~init:[]
      ~f:(fun acc mem ->
          let step = (Arch.addr_size arch :> size) in
          Memory.fold mem ~word_size:step ~init:[] ~f:(fun a w -> a :: w))

  let filter_plt addrs =
    section_memory ".plt" >>= fun memory ->
    Machine.return @@
    List.filter addrs ~f:(fun a ->
        Seq.exists memory ~f:(fun mem -> Memory.contains mem a))

  let unlink addrs =
    Machine.List.iter addrs ~f:(fun addr -> Linker.unlink (`addr addr))

  let unresolve  =
    load_table >>=
    filter_plt >>=
    unlink
end


module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Interpreter = Primus.Interpreter.Make(Machine)
  module Plt_jumps = Plt_jumps(Machine)

  let zero = Primus.Generator.static 0

  let initialize_flags flags =
    Machine.Seq.iter (Set.to_sequence flags) ~f:(fun reg ->
        Env.add reg zero)

  let correct_sp sp addend _ =
    Env.get sp >>= fun x ->
    Interpreter.binop Bil.plus x addend >>= Env.set sp

  let correct_sp sp addend =
    match Var.typ sp with
    | Unk | Mem _ -> Machine.return ()
    | Imm width ->
      Value.of_int ~width addend >>= fun addend ->
      Machine.sequence [
        Primus.Linker.Trace.lisp_call_return >>> correct_sp sp addend; ]

  let init () =
    Machine.get () >>= fun proj ->
    Machine.sequence @@
    match Project.arch proj with
    | `x86 ->
      [initialize_flags IA32.flags;
       correct_sp IA32.sp 4;
       Plt_jumps.unresolve ]
    | `x86_64 ->
      [initialize_flags AMD64.flags;
       correct_sp AMD64.sp 8;
       Plt_jumps.unresolve ]
    | _ -> []

end

let init () =
  let () = Bap_abi.register_pass Relink_plt.main in
  Primus.Machine.add_component (module Component)
