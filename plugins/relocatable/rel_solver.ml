open Core_kernel
open Bap.Std
open Monads.Std
module Fact = Ogre.Make(Monad.Ident)

include Self ()

let find_fixup jmp bounds rels exts =
  match Term.get_attr jmp address with
  | None -> None
  | Some addr ->
    match Map.find bounds addr with
    | None -> None
    | Some max_addr ->
      match Rel_data.find addr max_addr rels with
      | Some addr' -> Some (`Internal addr')
      | None ->
        match Rel_data.find addr max_addr exts with
        | None -> None
        | Some name -> Some (`External name)

let create_synthetic_sub name =
  let s = Sub.create ~name () in
  Tid.set_name (Term.tid s) name;
  Term.(set_attr s synthetic ())

let existing_subs prg =
  Seq.fold ~init:Addr.Map.empty
    (Term.to_sequence sub_t prg)
    ~f:(fun subs s ->
        match Term.get_attr s address with
        | None -> subs
        | Some a -> Map.add subs a s)

let solve insns rels exts prg =
  let existing = existing_subs prg in
  let synthetic = String.Table.create () in
  let get_by_addr = Map.find existing in
  let get_by_name name =
    Hashtbl.find_or_add synthetic name
      ~default:(fun () -> create_synthetic_sub name) in
  let bounds = Seq.fold insns ~init:Addr.Map.empty
      ~f:(fun mems (m,_) ->
          let min,max = Memory.(min_addr m, max_addr m) in
          Map.add mems min max) in
  let fixup jmp jmp_to =
    let target = match jmp_to with
      | `Internal addr -> get_by_addr addr
      | `External name -> Some (get_by_name name) in
    match target with
    | None -> jmp
    | Some sub ->
      let target = Direct (Term.tid sub) in
      match Jmp.kind jmp with
      | Call call ->
        let return = Call.return call in
        Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
      |  _ -> Jmp.with_kind jmp (Call (Call.create ~target ())) in
  let program = (object
    inherit Term.mapper
    method! map_jmp jmp =
      match find_fixup jmp bounds rels exts with
      | None -> jmp
      | Some jmp_to -> fixup jmp jmp_to
  end)#run prg in
  List.fold (Hashtbl.data synthetic) ~init:program
    ~f:(Term.append sub_t)

let run proj =
  match Project.get proj Image.specification with
  | None -> proj
  | Some spec ->
    match Rel_data.all spec with
    | Ok (rels, exts) ->
      let insns = Disasm.insns (Project.disasm proj) in
      let prog = solve insns rels exts (Project.program proj) in
      Project.with_program proj prog
    | Error er ->
      error "%a" Error.pp er;
      proj

let init () =
  Project.register_pass ~autorun:true ~name:"relocation_solver" run
