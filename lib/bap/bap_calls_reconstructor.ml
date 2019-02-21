open Core_kernel
open Bap_types.Std
open Bap_disasm_std
open Bap_sema.Std

module Cfg = Graphs.Cfg

let (>>=) = Option.bind

let fall_source symtab cfg s =
  Term.get_attr s address >>= fun addr ->
  Symtab.find_by_start symtab addr >>= fun (_,entry,_) ->
  Seq.find_map (Cfg.Node.inputs entry cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall -> Some (Cfg.Edge.src e)
      | _ -> None)

let collect_falls prog cfg symtab =
  Term.to_sequence sub_t prog |>
  Seq.fold ~init:Addr.Map.empty ~f:(fun calls s ->
      match fall_source symtab cfg s with
      | None -> calls
      | Some b -> Map.set calls (Block.addr b) (Term.tid s))

let call_of_jmp j =
  match Jmp.kind j with
  | Goto _ | Int _ | Ret _ -> None
  | Call call -> Some call

let call_target j =
  call_of_jmp j >>= fun call ->
  Some (Call.target call)

let update_return blk return_tid =
  Term.map jmp_t blk ~f:(fun jmp ->
      match call_of_jmp jmp with
      | None -> jmp
      | Some call -> match Call.return call with
        | Some _ -> jmp
        | None ->
          let call = Call.with_return call (Direct return_tid) in
          Jmp.with_kind jmp (Call call))

let create_call target =
  let b = Blk.Builder.create () in
  let call = Call.create ~target:(Label.direct target) () in
  let jmp = Jmp.create (Call call) in
  let () = Blk.Builder.add_jmp b jmp in
  Blk.Builder.result b

let call_exists b tid =
  Seq.exists (Blk.elts b)
    ~f:(function
        | `Def _ | `Phi _ -> false
        | `Jmp j -> match call_target j with
          | Some (Direct tid') -> Tid.(tid' = tid)
          | _ -> false)

module Bld = Sub.Builder

let rebuild_sub s calls =
  let bld = Bld.create ~name:(Sub.name s) ~tid:(Term.tid s) () in
  Seq.iter (Term.to_sequence blk_t s)
    ~f:(fun blk ->
        let elts = match Map.find calls (Term.tid blk) with
          | Some call -> [update_return blk (Term.tid call); call ]
          | None -> [blk] in
        List.iter elts ~f:(Bld.add_blk bld));
  let attrs = Term.attrs s in
  let s = Bld.result bld in
  Term.with_attrs s attrs

let update_sub falls s =
  let find_fall b = Term.get_attr b address >>= Map.find falls in
  let calls =
    Seq.fold (Term.to_sequence blk_t s) ~init:Tid.Map.empty
      ~f:(fun calls b ->
          match find_fall b with
          | None -> calls
          | Some tid when call_exists b tid -> calls
          | Some tid -> Map.set calls (Term.tid b) (create_call tid)) in
  if Map.is_empty calls then s
  else rebuild_sub s calls

let run cfg symtab prog =
  let falls = collect_falls prog cfg symtab in
  Term.map sub_t prog ~f:(update_sub falls)
