open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_future.Std
open Bap_sema.Std
open Bap_disasm_std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)

let of_seq s ~fkey ~fdata =
  Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
      Map.add m ~key:(fkey key) ~data:(fdata data))

module Rel = struct
  open Bap_image.Scheme
  open Fact.Syntax

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

let find insns data start =
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

let is_return insns addr =
  find_insn insns addr |> function
  | None -> false
  | Some (_, insn) -> Insn.(is return) insn

let fall_of_block cfg block =
  Seq.find_map (Cfg.Node.outputs block cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall -> Some (Cfg.Edge.dst e)
      | _ -> None)

let find_fall cfg insns addr =
  let blks = Cfg.nodes cfg in
  Seq.find blks ~f:(fun b ->
      Block.memory b |> Bap_memory.max_addr |> Addr.equal addr) |>
  function
  | None -> None
  | Some b ->
    fall_of_block cfg b

let relocate cfg insns rels exts pr =
  let subs = Term.to_sequence sub_t pr in
  let ext_subs = String.Table.create () in
  let find_sub = function
    | `Addr addr ->
      Seq.find subs ~f:(fun s -> match Term.get_attr s address with
          | None -> false
          | Some addr' -> Addr.equal addr addr')
    | `Name name ->
      Seq.find subs ~f:(fun s ->
          String.equal name (Sub.name s)) |> function
      | Some s -> Some s
      | None ->
        let () = Hashtbl.change ext_subs name ~f:(function
            | None -> Some (create_synthetic_sub name)
            | Some s -> Some s) in
        Hashtbl.find ext_subs name in
  let map_jmp ~local addr jmp jmp_to =
    match Jmp.kind jmp, find_sub jmp_to with
    | Call call, Some sub ->
      let return = Call.return call in
      let target = Direct (Term.tid sub) in
      Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
    | Goto (Indirect _), Some sub when local ->
      Jmp.with_kind jmp (Goto (Direct (Term.tid sub)))
    | Goto (Indirect _), Some sub ->
      let tid = Term.tid jmp in
      let target = Direct (Term.tid sub) in
      if is_return insns addr then
        Ir_jmp.create_ret ~tid target
      else
        let fall_b = find_fall cfg insns addr in
        Ir_jmp.create ~tid (Call (Call.create ~target ()))
    | _ -> jmp in
  let program = (object
    inherit Term.mapper
    method! map_jmp jmp =
      match Term.get_attr jmp address with
      | None -> jmp
      | Some addr ->
        match find insns rels addr with
        | None ->
          Option.value_map ~default:jmp (find insns exts addr)
            ~f:(fun name -> map_jmp ~local:false addr jmp (`Name name))
        | Some rel_addr -> map_jmp ~local:true addr jmp (`Addr rel_addr)
  end)#run pr in
  let append_ext prg sub = Term.append sub_t prg sub in
  List.fold ~init:program ~f:append_ext (Hashtbl.data ext_subs)

let run prog cfg insns spec =
  match Fact.eval Rel.relocations spec with
    | Error _ ->  prog
    | Ok (rels, exts) -> relocate cfg insns rels exts prog
