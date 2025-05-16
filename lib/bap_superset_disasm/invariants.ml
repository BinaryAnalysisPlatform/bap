open Bap.Std
open Core

module Dis = Disasm_expert.Basic

(** Build the static successors of the current instruction, and tag
    the superset with the function f in a micro-context. *)
let tag_with ~f (mem, insn) superset = 
  let targets = Superset.Inspection.static_successors superset mem insn in
  f superset mem insn targets

(** Looks for control flow edges to addresses that are not statically
    known. *)
let tag_target_not_in_mem superset mem insn targets =
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if not (Superset.Inspection.contains_addr superset target) then
            Superset.Core.mark_bad superset target
        | None -> ()
      );
  superset

(** If a jmp to NULL occurs, then this is triggered. *)
let tag_target_is_bad superset mem insn targets =
  let width = Addr.bitwidth @@ Memory.min_addr mem in
  let z = Addr.zero width in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if Addr.(target = z) then
            Superset.Core.mark_bad superset target
        | None -> ()
      );
  superset

(** If a jmp specifies the body of the instruction itself, this is
    triggered. *)
let tag_target_in_body superset mem insn targets =
  let src = Memory.min_addr mem in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if (Memory.contains mem target) && 
             not Addr.(src = target) then
            Superset.Core.mark_bad superset src
        | None -> ()
      );
  superset

(** Applies the tag together with the visitor and smaller functions
    under the hood. *)

(** If the instruction could not be disassembled or lifted, then tag
    it. *)
let tag_non_insn superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if Option.is_none insn then (
    Superset.Core.mark_bad superset src
  );
  superset

(** Used for the maintenance and construction of the superset. *)
let tag_success superset mem insn targets =
  let src = Memory.min_addr mem in
  List.fold targets ~init:superset ~f:(fun superset (target,_) -> 
      match target with
      | Some (target) -> 
        Superset.ISG.link superset target src
      | None -> superset)

let default_tags = [(*"Tag non insn", tag_non_insn;*)
                    (*"Tag target not in mem", tag_target_not_in_mem;*)
                    "Tag target is bad", tag_target_is_bad;
                    "Tag target in body", tag_target_in_body;
                    (*tag_success;*)]

let default_funcs = List.map default_tags ~f:snd

(** Tag an individual instruction of a superset with a list of
    invariants. *)
let tag ?invariants =
  let invariants = Option.value invariants ~default:default_funcs in
  let f superset mem insn targets =
    List.fold_left invariants ~init:superset ~f:(fun superset f -> 
        (f superset mem insn targets)) in
  tag_with ~f


(** Tag with a list of invariants over an entire superset. *)
let tag_superset ?invariants superset = 
  let invariants = Option.value invariants ~default:default_funcs in
  let f superset mem insn targets =
    List.fold ~init:superset invariants
      ~f:(fun superset invariant -> 
          invariant superset mem insn targets) in
  Superset.Core.fold ~init:superset superset ~f:(fun ~key ~data superset -> 
      let mem, insn = data in
      tag_with ~f (mem, insn) superset
    )
