open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

let get insns data start =
  let max_addr = Seq.find_map insns ~f:(fun (mem, _) ->
      let min, max = Memory.(min_addr mem, max_addr mem) in
      if Addr.equal min start then Some max
      else None) in
  match max_addr with
  | None -> None
  | Some max_addr ->
    Rel_fact.find start max_addr data

let synthetic_sub () =
  let s = Sub.create () in
  Term.(set_attr s synthetic ())

let spec = ref None

let set_spec s = spec := Some s

let relocate insns fact pr =
  let rels = Rel_fact.internals fact in
  let exts = Rel_fact.externals fact in
  let subs = Term.to_sequence sub_t pr in
  let find_sub = function
    | `Addr a ->
      Seq.find subs ~f:(fun s -> match Term.get_attr s address with
          | None -> false
          | Some a' -> Addr.equal a a')
    | `Name n ->
      Seq.find subs ~f:(fun s ->
          String.equal n (Sub.name s)) in
  let map_jmp jmp jmp_to =
    match Jmp.kind jmp, find_sub jmp_to with
    | Call call, Some sub ->
      let return = Call.return call in
      let target = Direct (Term.tid sub) in
      Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
    | Goto (Indirect _), Some sub ->
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
    match Rel_fact.create spec with
    | Error er -> error "%a" Error.pp er; proj
    | Ok fact ->
      let pr =
        Rel_fact.(to_seq (externals fact)) |>
        Seq.fold ~init:String.Set.empty
          ~f:(fun s (_,x) -> Set.add s x) |>
        Set.to_list |>
        List.fold ~init:(Project.program proj) ~f:(fun pr name ->
            let sub = Sub.create ~name () in
            let sub = Term.(set_attr sub synthetic ()) in
            Tid.set_name (Term.tid sub) name;
            Term.append sub_t pr sub) in
      let insns = Disasm.insns (Project.disasm proj) in
      let pr = relocate insns fact pr in
      Project.with_program proj pr

let init () =
  Stream.observe Project.Info.spec (fun s -> spec := Some s);
  Project.register_pass ~autorun:true main
