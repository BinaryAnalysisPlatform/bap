open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_future.Std
open Bap_sema.Std
open Bap_disasm_std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Bap_image.Scheme
  open Fact.Syntax

  let of_seq s ~fkey ~fdata =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key:(fkey key) ~data:(fdata data))

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let relocations =
    Fact.collect Ogre.Query.(select (from relocation))

  let external_symbols  =
    Fact.collect Ogre.Query.(
        select (from external_reference))

  let relocations =
    addr_width >>= fun width ->
    relocations >>= fun rels ->
    external_symbols >>= fun exts ->
    let to_addr = Addr.of_int64 ~width in
    Fact.return (of_seq rels ~fkey:to_addr ~fdata:to_addr,
                 of_seq exts ~fkey:to_addr ~fdata:ident)

end

let find_rel_data insns data start =
  let max_addr = Seq.find_map insns ~f:(fun (mem, _) ->
      let min, max = Bap_memory.(min_addr mem, max_addr mem) in
      if Addr.equal min start then Some max
      else None) in
  match max_addr with
  | None -> None
  | Some max_addr ->
    let rec get addr =
      if Addr.(addr > max_addr) then None
      else
        match Map.find data addr with
        | None -> get (Addr.succ addr)
        | Some value -> Some value in
    get start

let create_synthetic_sub name =
  let s = Sub.create ~name () in
  Tid.set_name (Term.tid s) name;
  Term.(set_attr s synthetic ())

let find_insn insns addr =
  Seq.find insns ~f:(fun (mem, _) ->
      Addr.equal (Bap_memory.min_addr mem) addr)

let fall_of_block cfg block =
  Seq.find_map (Cfg.Node.outputs block cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall -> Some (Cfg.Edge.dst e)
      | _ -> None)

let find_fall symtab subs tid addr =
  let open Option in
  Seq.find subs ~f:(fun s ->
      let blks = Term.to_sequence blk_t s in
      Seq.exists blks ~f:(fun b ->
          Option.is_some (Term.find jmp_t b tid))) >>= fun sub ->
  Symtab.find_by_name symtab (Sub.name sub) >>= fun (_,_,cfg) ->
  Seq.find (Cfg.nodes cfg)
    ~f:(fun b -> Bap_memory.contains (Block.memory b) addr) >>= fun b ->
  fall_of_block cfg b >>= fun fall ->
  let blks = Term.to_sequence blk_t sub in
  Seq.find blks ~f:(fun b ->
      match Term.get_attr b address with
      | None -> false
      | Some a -> Addr.equal (Block.addr fall) a) |> function
  | None -> Some (Label.indirect Bil.(int (Block.addr fall)))
  | Some blk -> Some (Label.direct (Term.tid blk))

let relocate symtab insns rels exts pr =
  let subs = Term.to_sequence sub_t pr in
  let ext_subs = String.Table.create () in
  let get_external name =
    Hashtbl.find_or_add ext_subs name
      ~default:(fun () -> create_synthetic_sub name) in
  let sub_of_name n =
    Seq.find subs ~f:(fun s -> String.equal n (Sub.name s)) in
  let sub_of_addr a =
    Seq.find subs ~f:(fun s -> match Term.get_attr s address with
        | None -> false
        | Some a' -> Addr.equal a a') in
  let get_sub = function
    | `Addr addr -> sub_of_addr addr
    | `Name name -> match sub_of_name name with
      | Some s -> Some s
      | None -> Some (get_external name) in
  let fixup addr jmp jmp_to =
    match Jmp.kind jmp, get_sub jmp_to with
    | Call call, Some sub ->
      let return = Call.return call in
      let target = Direct (Term.tid sub) in
      Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
    | _, Some sub  ->
      let tid = Term.tid jmp in
      let target = Direct (Term.tid sub) in
      let return = find_fall symtab subs tid addr in
      Ir_jmp.create ~tid (Call (Call.create ~target ?return ()))
    | _ -> jmp in
  let program = (object
    inherit Term.mapper
    method! map_jmp jmp =
      match Term.get_attr jmp address with
      | None -> jmp
      | Some addr ->
        match find_rel_data insns rels addr with
        | None ->
          Option.value_map ~default:jmp
            (find_rel_data insns exts addr)
            ~f:(fun name  -> fixup addr jmp (`Name name))
        | Some rel_addr -> fixup addr jmp (`Addr rel_addr)
  end)#run pr in
  let append_ext prg sub = Term.append sub_t prg sub in
  List.fold ~init:program ~f:append_ext (Hashtbl.data ext_subs)

let run prog symtab insns spec =
  match Fact.eval Rel.relocations spec with
  | Error _ ->  prog
  | Ok (rels, exts) -> relocate symtab insns rels exts prog
