open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Image
open Monads.Std

include Self ()

module Fact = Ogre.Make(Monad.Ident)

let of_aseq width x =
  Seq.fold x ~init:Addr.Map.empty ~f:(fun m (key,data) ->
      let key = Addr.of_int64 ~width key in
      Map.add m ~key ~data)

module Rel = struct
  open Image.Scheme
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
    external_symbols >>= fun ext ->
    Fact.return (of_aseq width rels, of_aseq width ext)

end

let get insns data start =
  let max_addr = Seq.find_map insns ~f:(fun (mem, _) ->
      let min, max = Memory.(min_addr mem, max_addr mem) in
      if Addr.equal min start then Some max
      else None) in
  match max_addr with
  | None -> None
  | Some max_addr ->
  let rec find addr =
    if Addr.(addr > max_addr) then None
    else
      match Map.find data addr with
      | None -> find (Addr.succ addr)
      | Some value -> Some value in
  find start

let synthetic_sub () =
  let s = Sub.create () in
  Term.(set_attr s synthetic ())

let spec = ref None

let relocate insns rels exts pr =
  let subs = Term.to_sequence sub_t pr in
  let find_sub = function
    | `Addr a ->
      Seq.find subs ~f:(fun s -> match Term.get_attr s address with
          | None -> false
          | Some a' ->
            Addr.of_int64 ~width:(Addr.bitwidth a') a |>
            Addr.equal a')
    | `Name n ->
      Seq.find subs ~f:(fun s ->
          String.equal n (Sub.name s)) in
  let map_jmp jmp jmp_to =
    match Jmp.kind jmp, find_sub jmp_to with
    | Call call, Some sub ->
      let return = Call.return call in
      let target = Direct (Term.tid sub) in
      Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
    | Goto (Indirect addr), Some sub ->
      Jmp.with_kind jmp (Goto (Direct (Term.tid sub)))
    | _ -> jmp in
  (object
    inherit Term.mapper
    method! map_jmp jmp =
      match Term.get_attr jmp address with
      | None -> jmp
      | Some addr ->
        match get insns rels addr with
        | None ->
          Option.value_map ~default:jmp (get insns exts addr)
            ~f:(fun name -> map_jmp jmp (`Name name))
        | Some rel_addr -> map_jmp jmp (`Addr rel_addr)
  end)#run pr

let main proj =
  match !spec with
  | None -> proj
  | Some spec ->
    match Fact.eval Rel.relocations spec with
    | Error er -> error "%a" Error.pp er; proj
    | Ok (rels, exts) ->
      let pr =
        String.Set.of_list (Map.data exts) |>
        Set.to_list |>
        List.fold ~init:(Project.program proj) ~f:(fun pr name ->
            let sub = Sub.create ~name () in
            let sub = Term.(set_attr sub synthetic ()) in
            Tid.set_name (Term.tid sub) name;
            Term.append sub_t pr sub) in
      let insns = Disasm.insns (Project.disasm proj) in
      let pr = relocate insns rels exts pr in
      Project.with_program proj pr

let init () =
  Stream.observe Project.Info.spec (fun s -> spec := Some s);
  Project.register_pass ~autorun:true main
