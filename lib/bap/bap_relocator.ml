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
    Fact.collect Ogre.Query.(select (from external_reference))

  let relocations =
    addr_width >>= fun width ->
    relocations >>= fun rels ->
    external_symbols >>= fun exts ->
    let to_addr = Addr.of_int64 ~width in
    Fact.return (of_seq rels ~fkey:to_addr ~fdata:to_addr,
                 of_seq exts ~fkey:to_addr ~fdata:ident)

end

let find_fixup (relocations, externals) min_addr max_addr =
  let find data =
    let rec get addr =
      if Addr.(addr > max_addr) then None
      else
        match Map.find data addr with
        | None -> get (Addr.succ addr)
        | Some value -> Some value in
    get min_addr in
  match find relocations with
  | Some rel_addr -> Some (`Addr rel_addr)
  | None -> match find externals with
    | Some name -> Some (`Name name)
    | None -> None

let create_synthetic_sub name =
  let s = Sub.create ~name () in
  Tid.set_name (Term.tid s) name;
  Term.(set_attr s synthetic ())

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

let relocate symtab insns fixups pr =
  let subs = Term.to_sequence sub_t pr in
  let ext_subs = String.Table.create () in
  let memory = Seq.fold insns ~init:Addr.Map.empty
      ~f:(fun mems (m,_) ->
          let min,max = Bap_memory.(min_addr m, max_addr m) in
          Map.add mems min max) in
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
    | _, Some sub ->
      let tid = Term.tid jmp in
      let target = Direct (Term.tid sub) in
      let return = find_fall symtab subs tid addr in
      Ir_jmp.create ~tid (Call (Call.create ~target ?return ()))
    | _ -> jmp in
  let (>>=) = Option.(>>=) in
  let program = (object
    inherit Term.mapper
    method! map_jmp jmp =
      Option.value ~default:jmp
        (Term.get_attr jmp address >>= fun addr ->
         Map.find memory addr >>= fun max_addr ->
         find_fixup fixups addr max_addr >>= fun fix ->
         Some (fixup addr jmp fix))
  end)#run pr in
  let append_ext prg sub = Term.append sub_t prg sub in
  List.fold ~init:program ~f:append_ext (Hashtbl.data ext_subs)

let run prog symtab insns spec =
  match Fact.eval Rel.relocations spec with
  | Error _ ->  prog
  | Ok fixups -> relocate symtab insns fixups prog
