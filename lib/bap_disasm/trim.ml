open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_target_factory

module Insn    = Bap_disasm_insn
module Brancher = Bap_disasm_brancher

let static_successors brancher mem insn =
  match insn with 
  | None -> [None, `Fall]
  | Some insn -> 
    try 
      Brancher.resolve brancher mem insn
    with _ -> (
        print_endline @@ 
        "Target resolve failed on memory at " ^ Memory.to_string mem; 
        [None, `Fall]
      )

let find_non_mem_accesses superset = 
  let check_return_addr r addr = 
    match addr with
    | Bil.Int(addr) -> 
      if Superset.contains_addr superset addr then
        r
      else r.return(Some(false))
    | _ -> r in
  (object(self) 
    inherit [bool] Stmt.finder
    method! enter_load ~mem ~addr _ _ r = 
      check_return_addr r addr
    method! enter_store ~mem ~addr ~exp _ _ r =
      check_return_addr r addr
  end)

let accesses_non_mem superset mem insn _ = 
  let arch = Superset.get_arch superset in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  try
    let bil = Superset.lift_insn lifter (mem, insn) in
    let _, bil = Option.value ~default:(mem,[]) bil in
    let status = List.fold bil ~init:None ~f:(fun status _stmt -> 
        Option.value_map status ~default:(Some(false)) ~f:(fun status ->
            if not status then
              Stmt.find (find_non_mem_accesses superset) _stmt
            else Some(status)
          )
      ) in
    Option.value status ~default:false
  with _ -> false 

(* TODO Does this belong in Superset? *)
let tag_with ~f (mem, insn) superset = 
  let open Superset in
  let targets = static_successors superset.brancher mem insn in
  f superset mem insn targets

let tag_target_not_in_mem superset mem insn targets =
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if not (Superset.contains_addr superset target) then
            Superset.mark_bad superset target
        | None -> ()
      );
  superset

let tag_target_is_bad superset mem insn targets =
  let width = Addr.bitwidth @@ Memory.min_addr mem in
  let z = Addr.zero width in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if Addr.(target = z) then
            Superset.mark_bad superset target
        | None -> ()
      );
  superset

(* TODO need to add a unit test *)
let tag_target_in_body superset mem insn targets =
  let src = Memory.min_addr mem in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if (Memory.contains mem target) && 
             not Addr.(src = target) then
            Superset.mark_bad superset src
        | None -> ()
      );
  superset

let tag_invalid_targets superset mem insn targets = 
  let superset = tag_target_not_in_mem superset mem insn targets in
  let superset = tag_target_is_bad superset mem insn targets in
  let superset = tag_target_in_body superset mem insn targets in
  superset

let tag_non_mem_access superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if accesses_non_mem superset mem insn targets then (
    (* The instruction reads or writes to memory that is not mapped *)
    Superset.mark_bad superset src
  );
  superset

let tag_non_insn superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if Option.is_none insn then (
    (* Wasn't a parseable instruction *)
    Superset.mark_bad superset src
  );
  superset


(* TODO This belongs in Superset *)
(* could merge add_to_map and tag_success *)
let tag_success superset mem insn targets =
  let src = Memory.min_addr mem in
  let insn_risg = Superset.get_graph superset in
  (*let superset = Superset.add superset mem insn in*)
  (* TODO perhaps the below should be merged with Superset.add *)
  List.iter targets ~f:(fun (target,_) -> 
      match target with
      | Some (target) -> 
        Superset_risg.G.add_edge insn_risg target src
      | None -> ());
  superset

let default_tags = ["Tag non insn", tag_non_insn;
                    "Tag non mem access", tag_non_mem_access;
                    "Tag target not in mem", tag_target_not_in_mem;
                    "Tag target is bad", tag_target_is_bad;
                    "Tag target in body", tag_target_in_body;
                    (*tag_success;*)]

let default_funcs = [
  tag_non_insn;
  tag_non_mem_access;
  tag_target_not_in_mem;
  tag_target_is_bad;
  tag_target_in_body;
  (*tag_success;*)]

let tag ?invariants =
  let invariants = Option.value invariants ~default:default_funcs in
  let f superset mem insn targets =
    List.fold_left invariants ~init:superset ~f:(fun superset f -> 
        (f superset mem insn targets)) in
  tag_with ~f

module type Reducer = sig
  type acc
  val accu : acc
  val check_pre : 'a Superset.t -> acc -> addr -> acc
  val check_elim : 'a Superset.t -> acc -> addr -> bool
  val check_post : 'a Superset.t -> acc -> addr -> acc
  val mark  : 'a Superset.t -> acc -> addr -> unit
end

module type ReducerInstance = sig
  include Reducer
  val post : 'a Superset.t -> acc -> addr -> acc
end

module Reduction(R : Reducer) = struct

  let post superset accu addr =
    let module G = Superset_risg.G in
    let superset_risg = Superset.get_graph superset in
    if R.check_elim superset accu addr then (
      R.mark superset accu addr;
      G.remove_vertex superset_risg addr;
    );
    R.check_post superset accu addr

  let trim superset =
    print_endline "trimming...";
    let superset_risg = Superset.get_graph superset in
    let module G = Superset_risg.G in
    let superset = Superset.rebalance superset in
    let orig_size = (G.nb_vertex superset_risg) in
    let post = post superset in
    let pre = R.check_pre superset in
    let _ = Superset.with_bad superset ~pre ~post R.accu in
    Superset.clear_all_bad superset;
    let trimmed_size = (G.nb_vertex superset_risg) in
    let num_removed = orig_size - trimmed_size in
    printf "%d vertices after trimming, removing %d\n" 
      trimmed_size num_removed;
    Superset.rebalance superset

end

module Disabled = struct
  let post _ accu _ = accu
  let trim superset = superset
end

module DefaultReducer = Reduction(struct
    type acc = unit
    let accu = ()
    let check_pre _ accu _ = accu
    let check_post _ accu _ = accu
    let check_elim _ _ _ = true
    let mark _ _ _ = ()
  end)

module Default = DefaultReducer

module DeadblockTolerantReducer : Reducer
(*with type acc = Superset.elem option*) = struct
  type acc = Superset.elem option
  let accu = None
  let check_pre superset accu addr =
    match accu with 
    | Some _ -> accu
    | None -> (
        match Map.find Superset.(get_map superset) addr with
        | Some (mem,insn) -> (
            match insn with
            | Some i -> 
              let i = Insn.of_basic i in
              if Insn.may Insn.affect_control_flow i then
                Some (mem,insn) else None
            | None -> None
          )
        | None -> None
      )

  let check_post superset accu addr =
    match accu with
    | Some(mem,insn) ->
      if Memory.(min_addr mem) = addr then
        None
      else accu
    | None -> accu

  let check_elim superset accu addr =
    Option.is_none accu

  let mark _ _ _ = ()
end

module DeadblockTolerant = Reduction(DeadblockTolerantReducer)

let tag_superset ?invariants superset = 
  let invariants = Option.value invariants ~default:default_funcs in
  let insn_map = Superset.get_map superset in
  let f superset mem insn targets =
    List.fold ~init:superset invariants
      ~f:(fun superset invariant -> 
          invariant superset mem insn targets) in
  Addr.Map.fold ~init:superset insn_map ~f:(fun ~key ~data superset -> 
      let mem, insn = data in
      tag_with ~f (mem, insn) superset
    )

let tagged_disasm_of_file ~data ?f ?invariants ~backend file =
  let invariants = Option.value invariants ~default:default_funcs in
  let f = Option.value f ~default:[] in
  let invariants = Some(List.append f invariants) in
  Superset.superset_disasm_of_file ~data ~backend file ~f:(tag ?invariants)

let trimmed_disasm_of_file ~data ?f ~backend file =
  Default.trim (tagged_disasm_of_file ~data ?f ~backend file)
