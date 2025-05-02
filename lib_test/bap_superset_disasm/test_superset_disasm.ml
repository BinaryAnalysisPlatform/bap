open OUnit2
open Core_kernel
open Bap.Std
open Or_error
open Superset
open Bap_plugins.Std
open Graphlib.Std
open Bap_future.Std

let requires = ["llvm"; "lifter"; "disassemble"; "disassembler";
                "semantics"]
let () = match Bap_main.init ~requires () with
  | Ok () -> ()
  | Error err -> 
     let open Bap_main in
     Bap_main.Extension.Error.pp Format.std_formatter err;
     exit 1

let create_memory arch min_addr data =
  let data = Bigstring.of_string data in
  Memory.create (Arch.endian arch) min_addr data

let arch = `x86

let segments = Table.empty

let width = 8*(Arch.size_in_bytes arch);;
let zero = Addr.(of_int ~width 0)

module G = Graphlib.Make(Addr)(Unit)
module Topological = Superset_impl.Topological

let add_edge g v1 v2 = 
  let e = G.Edge.create v1 v2 () in
  G.Edge.insert e g

let mem_edge g v1 v2 =
  let e = G.Edge.create v1 v2 () in
  G.Edge.mem e g

let init () = 
  let insn_isg = Graphlib.create (module G) () in
  let insn_map = Addr.Map.empty in
  insn_map, insn_isg

let min_addr = 1
let addr_size= Size.in_bits @@ Arch.addr_size arch
let min_addr = Addr.of_int ~width:addr_size min_addr

let make_params ?(mina=min_addr) bytes =
  let memory = create_memory arch min_addr bytes |> ok_exn in
  memory, arch

let check_results sizes expected_results = 
  let sizes = Seq.to_list sizes in
  List.iter2_exn sizes expected_results
    ~f:(fun actual_size expected_size ->
        assert_equal ~msg:((List.to_string ~f:string_of_int sizes)
                           ^ (List.to_string ~f:string_of_int
                                expected_results)) actual_size
          expected_size)  

let superset_to_length_list superset =
  List.map superset
    ~f:(fun (mem, insn) -> (Memory.length mem))

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the superset conservative disassembler *)
let test_hits_every_byte test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  let raw_superset = Superset.Core.disasm_all
      ~accu:[] ~f:List.cons arch memory |> ok_exn in
  let sizes = superset_to_length_list raw_superset in
  let expected_results = List.rev [ 5; 2; 1; 1; 1; ] in
  check_results (Seq.of_list sizes) expected_results

let of_mem arch mem = 
  let superset = Superset.Core.empty arch in
  let f = (Invariants.tag ~invariants:[Invariants.tag_success]) in
  Superset.Core.update_with_mem superset mem ~f

let get_bads superset mem =
  let maddr  = Memory.min_addr mem in
  let l = Memory.length mem in
  let bds = Superset.Core.seq_of_addr_range maddr l in
  Seq.filter bds
    ~f:(Superset.Inspection.is_bad_at superset)

let str_of_bads superset mem =
  let bds = get_bads superset mem in
  let bds = Seq.to_list bds in
  List.to_string ~f:Addr.to_string bds

let debug_msg superset mem =
  let msg = Superset.ISG.isg_to_string superset in
  let bads = str_of_bads superset mem in
  let msg = sprintf "%s\n%s"
      msg bads in
  (*let pattern = ": " in
    let msi =
    String.substr_index mems ~pattern |> Option.value_exn in
    let start = (msi+(String.length pattern)) in
    let finish = String.((length mems)) - start in
    let ms = String.sub mems start finish in*)
  let cnt = Superset.Inspection.count superset in
  let unb = Superset.Inspection.count_unbalanced superset in
  sprintf "%s\ncount (graph): %d, unbalanced (map): %d" msg cnt unb

(* TODO make default initialization from bytes much shorter *)

let test_trim test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let superset =
    Invariants.tag_superset superset in
  let superset = Trim.run superset in
  let superset = Superset.Core.rebalance superset in
  let dbg = debug_msg superset mem in
  let bads = str_of_bads superset mem in
  let explanation =
    sprintf "Expect one instruction, got %d. bad: %s"
      (Superset.Inspection.count superset) bads in
  let msg = sprintf "%s\n%s"
      dbg explanation in   
  (* Only the return opcode ( 0xc3 ) can survive trimming *)
  (* After refactoring, it may be that some targets that fall outside
     the memory bounds are still in the graph, but this only accounts
     for one edge, so it is negligible. *)
  assert_equal ~msg 1 @@ Superset.Inspection.count superset

let test_can_lift test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  try
    Superset.Core.fold superset ~init:() ~f:(fun ~key ~data () ->
        let (mem, insn) = data in
        let bil = Superset.Core.lift_insn superset (mem, insn) in
        let msg =
          sprintf "couldn't lift at %s" (Memory.to_string mem) in
        match bil with
        | Some _ -> ()
        | _ ->
          assert_bool msg false
      ) 
  with _ -> ()  

let test_brancher test_ctxt =
  let bytes = "\x77\x77" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  match Superset.Core.lookup superset Memory.(min_addr mem) with
  | Some (mem, insn) -> 
     let ss =
       Superset.Inspection.static_successors superset mem insn in
     let msg = sprintf "Should be two static successors here" in                 
     assert_bool msg (List.(length ss) > 1)
  | None -> assert_bool "should be an instruction at 0" false

let test_lift test_ctxt =
  let bytes = "\x77\x77" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  match Superset.Core.lookup superset Memory.(min_addr mem) with
  | Some (mem, insn) -> 
     let lifted =
       Superset.Core.lift_insn superset (mem, insn) in
     let msg = sprintf "Should be able to lift" in
     assert_bool msg Option.(is_some lifted);
     let r = Option.value_map ~default:false lifted ~f:(fun (bil) ->
                 List.length bil > 0) in
     assert_bool "Should support this!" r
  | None -> assert_bool "should be an instruction at 0" false

(* TODO want a bil language oriented way to specify the construction of a superset *)
let dis_with_invariants ?superset bytes invariants =
  let mem, arch = make_params bytes in
  let default =
    of_mem arch mem in
  let superset = Option.value superset ~default in
  let f = (Invariants.tag ~invariants) in
  let superset = Superset.Core.update_with_mem
                   superset mem ~f in
  let msg = debug_msg superset mem in
  let msg = sprintf "Should be bad at %s\n%s"
              Addr.(to_string min_addr) msg in
  let superset = Trim.run superset in
  let superset = Superset.Core.rebalance superset in
  assert_bool msg @@ not @@ Superset.Core.mem superset min_addr;
  let msg = debug_msg superset mem in
  let offset_one =
    Superset.Core.mem superset min_addr in
  assert_bool msg (not offset_one)

let test_tag_non_insn test_ctxt =
  dis_with_invariants "\x0f\xff"
    [Invariants.tag_success; Invariants.tag_non_insn]

let test_tag_target_is_bad test_ctxt = ()
(* TODO don't want to use this until have functor on superset
 * interface allowing to set the semantics directly for test *)
  (*let bytes = "\x77\xfe" in
  dis_with_invariants bytes
    [Invariants.tag_success; Invariants.tag_target_is_bad]*)

let test_target_in_body test_ctxt =
  let bytes = "\x77\xFF" in
  dis_with_invariants bytes
    [Invariants.tag_success; Invariants.tag_target_in_body]

let test_target_not_in_mem test_ctxt = 
  let bytes = "\x77\x77" in
  let invariants = [Invariants.tag_success;
                    Invariants.tag_target_not_in_mem] in
  dis_with_invariants bytes invariants
  
let test_static_successors_includes_fall_through test_ctxt =
  let bytes = "\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let maddr = Memory.(min_addr mem) in
  match Superset.Core.lookup superset maddr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr -> status || Addr.(addr = (succ maddr))
        | None -> status 
      ) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false


let test_successor_calculation test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let mn_addr = Memory.(min_addr mem) in
  let mx_addr = Memory.(max_addr mem) in
  match Superset.Core.lookup superset mn_addr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr ->
          status ||
          (Addr.(addr > (mx_addr)) && (not Memory.(contains mem addr)))
        | None -> status 
      ) in
    let b = b && (List.length tgts > 0) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false

let test_superset_contains_addr test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let mn_addr = Memory.(min_addr mem) in
  match Superset.Core.lookup superset mn_addr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr ->
          status ||
          (not (Superset.Inspection.contains_addr superset addr))
        | None -> status 
      ) in
    let b = b && (List.length tgts > 0) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false

let test_trims_invalid_jump test_ctxt =
  let bytes = "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let memory, arch = make_params bytes in
  let superset = of_mem arch memory in
  let superset = Superset.Core.update_with_mem
      superset memory ~f:Invariants.tag  in
  let superset = Trim.run superset in
  let superset = Superset.Core.rebalance superset in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal"
    (Superset.Inspection.count superset)
    (List.length expected_results)

let test_addr_map test_ctxt =
  let min_addr  = Addr.of_int ~width:addr_size 0 in
  let insn_map  = Addr.Map.empty in
  let insn_map  = Addr.Map.set insn_map ~key:min_addr ~data:() in
  let insn_map  = Addr.Map.set insn_map ~key:min_addr ~data:() in
  let msg = "expected length to be one" in
  assert_bool msg ((Addr.Map.length insn_map) = 1)

let test_insn_isg test_ctxt = 
  let insn_risg = Graphlib.create (module G) () in
  let addr  = Addr.of_int ~width:addr_size 0 in
  let insn_risg = G.Node.insert addr insn_risg in
  let insn_risg = G.Node.insert addr insn_risg in
  let msg = "expected length to be one" in
  assert_bool msg ((G.number_of_nodes insn_risg) = 1)  

let test_consistent_superset test_ctxt = 
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let superset = of_mem arch memory in
  let m_neg_g, g_neg_m = Superset.Inspection.unbalanced_diff superset in
  let msg = "insn in map but not in after shingled of superset" in
  assert_bool msg (Set.is_empty m_neg_g);
  let msg = "insn in isg but not in map after shingled of superset" in
  assert_bool msg (Set.is_empty g_neg_m)

let construct_loop insn_map insn_isg start finish = 
  if Addr.(finish > start) then (
    (* Where we would otherwise connect the nodes from the tail
       condition back up to the loop body entry, here all the edges are
       reversed in the spirit of the disassembled insn_isg. *)
    let insn_isg = add_edge insn_isg start finish in
    let junk_data = String.of_char ' ' in
    let start_mem = create_memory arch start junk_data |> ok_exn in
    let insn_map = Addr.Map.set insn_map ~key:start ~data:(start_mem, None) in
    let finish_mem = create_memory arch finish junk_data |> ok_exn in
    let insn_map = Addr.Map.set insn_map ~key:finish ~data:(finish_mem, None) in
    let one  = (Addr.of_int 1 ~width) in
    let two  = (Addr.of_int 2 ~width) in
    let rec construct_loop_body insn_isg insn_map start finish = 
      if not (Addr.equal start finish) then
        let dist = Addr.max Addr.((finish - start)/two) one in
        let step = Addr.(start + dist) in
        (* Add edge from some intermediate point between the start and
           the finish going from the calculated step toward the parameter
           start, decreasing the distance between the outermost start
           and finish. As this function executes, it creates log(dist)
           nodes, each going from finish to step (reverse as it would
           be a flow in a real binary) before finally reaching the
           start *)
        let insn_isg = add_edge insn_isg step start in
        (* Because the algorithm at this point relies on the graph
           and map entirely, it doesn't matter the contents of the
           memory. *)
        let junk_data = String.make (Addr.to_int dist |> ok_exn) ' ' in
        let insn_map = Addr.Map.set insn_map ~key:start
            ~data:(create_memory arch start junk_data |> ok_exn, None) in
        construct_loop_body insn_isg insn_map step finish 
      else insn_map, insn_isg in
    construct_loop_body insn_isg insn_map start finish
  ) else insn_map, insn_isg

let construct_entry_conflict insn_map insn_isg at conflict_len = 
  let junk_data = String.make conflict_len ' ' in
  let conflict = Addr.(at ++ conflict_len) in
  let insn_isg = add_edge insn_isg at conflict in
  let insn_isg = add_edge insn_isg
                   Addr.(at ++ 1) Addr.(conflict ++ 1) in
  let insn_map = Addr.Map.set insn_map ~key:at 
      ~data:(create_memory arch at junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:conflict
      ~data:(create_memory arch conflict junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:Addr.(conflict ++1)
      ~data:(create_memory arch Addr.(conflict ++1) junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data |> ok_exn, None) in
  insn_map, insn_isg

let construct_tail_conflict 
    insn_map insn_isg tail_addr conflict_count =
  let orig = 
    if G.Node.mem tail_addr insn_isg &&
       G.Node.degree ~dir:`Out tail_addr insn_isg > 0 then
      let orig = G.Node.succs tail_addr insn_isg in
      let orig = Addr.Set.of_list @@ Seq.to_list orig in orig
    else Addr.Set.empty in
  let insn_map, tail_len = match Addr.Map.find insn_map tail_addr with
    | Some (mem, _) -> insn_map, Memory.length mem
    | None -> 
      let tail_len = 1 in
      let tail_data = String.make tail_len ' ' in
      let mem = create_memory arch tail_addr tail_data |> ok_exn in
      let insn_map = Addr.Map.set insn_map ~key:tail_addr
          ~data:(mem, None) in
      insn_map, tail_len in 
  let rec make_tail_options insn_map insn_isg conflict_count =
    if conflict_count > 0 then
      let junk_data = String.make conflict_count ' ' in
      let conflict_addr = Addr.(tail_addr -- conflict_count) in
      let insn_isg = add_edge insn_isg tail_addr conflict_addr in
      let insn_map = Addr.Map.set insn_map ~key:conflict_addr 
          ~data:(create_memory arch conflict_addr junk_data |> ok_exn,
                 None) in
      make_tail_options insn_map insn_isg (conflict_count - 1)
    else 
      insn_map, insn_isg in
  let insn_map, insn_isg = 
    make_tail_options insn_map insn_isg conflict_count in
  let opts = G.Node.succs tail_addr insn_isg in
  let opts = Addr.Set.of_list @@ Seq.to_list opts in
  let opts = Set.diff opts orig in
  let msg = sprintf
      "expected %d, got %d" conflict_count Set.(length opts) in
  assert_equal ~msg true (Set.(length opts) = conflict_count);
  insn_map, insn_isg

let test_loop_scc test_ctxt = 
  let insn_map, insn_isg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_isg = 
    construct_loop insn_map insn_isg entry Addr.(entry ++ 20) in
  let loop_points = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_isg)
    ~f:(fun vert -> Hash_set.add loop_points vert);
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg:insn_isg arch in
  let scc = Superset.ISG.raw_loops superset in
  let scc_points = Addr.Hash_set.create () in
  List.iter scc ~f:(fun scc -> 
      List.iter scc ~f:(fun component_addr -> 
          Hash_set.add scc_points component_addr;
        ));
  let in_loop_not_scc = "Found addr in loop but not in scc" in
  Hash_set.iter loop_points ~f:(fun loop_addr ->
      assert_equal ~msg:in_loop_not_scc true
      @@ Hash_set.mem scc_points loop_addr);
  let in_scc_not_loop = "Found addr in scc not loop" in
  Hash_set.iter scc_points ~f:(fun loop_addr ->
      assert_equal ~msg:in_scc_not_loop true
      @@ Hash_set.mem loop_points loop_addr)

let test_scc test_ctxt =
  let insn_map, insn_risg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  let insn_risg = add_edge insn_risg zero entry in
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg arch in
  let components = Grammar.addrs_of_filtered_loops superset in
  assert_equal ~msg:"found non scc component" 0 (Set.length components)

let test_find_conflicts test_ctxt = 
  let insn_map, insn_risg = init () in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let num_conflicts = 6 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg in_loop_addr
      num_conflicts in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in 
  assert_equal Set.(length conflicts) num_conflicts

let test_trim_scc test_ctxt = 
  let insn_map, insn_risg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_risg = 
    construct_loop insn_map insn_risg entry Addr.(entry ++ 20) in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in
  let keep_entry = Superset.Core.mem superset entry in
  let msg = sprintf "entry %s should be in the graph"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry true;
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let loop_points = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_risg) ~f:(Hash_set.add loop_points);
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg in_loop_addr 3 in
  let conflicts_added = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_risg) ~f:(fun vert -> 
      if not (Hash_set.mem loop_points vert) then 
        Hash_set.add conflicts_added vert);
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in
  let components = Superset.ISG.raw_loops superset in
  assert_bool "Should have a component" 
    (List.(length components) > 0);
  let superset = 
    Grammar.tag_loop_contradictions ~min_size:1 superset in
  assert_bool "should have marked conflict" 
    (0 < Superset.Inspection.(num_bad superset));
  let keep_entry = Superset.Inspection.is_bad_at superset entry in
  let msg = sprintf "entry %s should not be marked bad"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry false;
  let superset = Trim.run superset in
  let keep_entry = Superset.Core.mem superset entry in
  let msg = sprintf "entry %s should not be removed"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry true;
  let conflicts_added_str = List.to_string ~f:Addr.to_string @@ 
                              Hash_set.to_list conflicts_added in
  let removed_msg = "of conflicts " ^ conflicts_added_str 
                    ^ ", residual conflict present within " in
  let isg_msg = Superset.ISG.isg_to_string superset in
  let removed_msg = sprintf "%s\n%s" isg_msg removed_msg in
  let (mlg, glm) = Superset.Inspection.unbalanced_diff superset in
  let msg = sprintf "%s\nmlg: %d, glm: %d" removed_msg
              Set.(length mlg) Set.(length glm) in
  let set_to_string s =
    List.to_string ~f:Addr.to_string @@ Set.to_list s in
  let msg = sprintf "%s\nmap less graph: %s, graph less map: %s"
              msg (set_to_string mlg) (set_to_string glm) in
  assert_bool msg (Set.(length mlg)=0 && Set.(length glm)=0);
  let loop_msg addr =
    sprintf "%s\nloop addr %s should remain during tail trim"
      (Superset.ISG.isg_to_string superset)
      Addr.(to_string addr) in
  Hash_set.iter loop_points ~f:(fun addr -> 
      assert_equal ~msg:(loop_msg addr) true @@ 
      Superset.Core.mem superset addr)

(* Establishes, in the case of if structures, how topological *)
(* tranversal works - one time visit only *)
let test_topological_revisit ctxt = 
  let _, insn_risg = init () in
  let width = 32 in
  let start = Addr.of_int 0 ~width in
  let stop = Addr.of_int 2 ~width in
  let rec make_if insn_risg current stop =
    if not Addr.(current = stop) then
      let next = Addr.succ current in
      let insn_risg = add_edge insn_risg current next in
      make_if insn_risg next stop else insn_risg in
  let insn_risg = make_if insn_risg start stop in
  let insn_risg = add_edge insn_risg start stop in
  let update_count addr visit_count = 
    match Map.find visit_count addr with
    | Some (count) -> 
      let visit_count = Map.remove visit_count addr in
      Map.set visit_count ~key:addr ~data:(count+1)
    | None -> Map.set visit_count ~key:addr ~data:1 in
  
  let visit_count = Topological.fold 
      update_count insn_risg Addr.Map.empty in
  Map.iteri visit_count ~f:(fun ~key ~data -> assert_equal ~ctxt data 1)

let rec extend_back insn_map insn_isg ?(step=1) addr num =
  let make_link len =
    let dest = Addr.(addr -- len) in
    let insn_isg = add_edge insn_isg addr dest in
    let junk_data = String.make len ' ' in
    let mem = create_memory arch dest junk_data |> ok_exn in
    let insn_map = Map.set insn_map ~key:dest ~data:(mem, None) in
    (insn_map, insn_isg) in
  if not (num = 0) then 
    let (insn_map, insn_isg) = make_link step in
    extend_back insn_map insn_isg Addr.(addr -- step) (num - 1) ~step
  else
    insn_map, insn_isg

let make_extended_cross tail_addr =
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg tail_addr 2 in
  let layer_options = Seq.to_list G.Node.(succs tail_addr insn_risg) in
  let insn_map, insn_risg = List.fold ~init:(insn_map, insn_risg)
      layer_options ~f:(fun (insn_map, insn_risg) opt -> 
          extend_back insn_map insn_risg opt 1 ~step:2
      ) in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in  
  let extended_points = 
    Superset.entries_of_isg superset in
  let _,insn_risg = Hash_set.fold ~init:(None,insn_risg) extended_points
      ~f:(fun (current,insn_risg) next -> 
          let insn_risg = Option.value_map current ~f:(fun current -> 
              add_edge insn_risg current next
            ) ~default:insn_risg in
          Some(next),insn_risg
        ) in
  insn_map, insn_risg

let construct_branch insn_map insn_risg branch_at incr = 
  let left = Addr.(branch_at ++ incr) in
  let junk_data = String.make incr ' ' in
  let left_mem = create_memory arch left junk_data |> ok_exn in
  let insn_map = Map.set insn_map ~key:left ~data:(left_mem, None) in
  let right = Addr.(left ++ incr) in
  let right_mem = create_memory arch right junk_data |> ok_exn in
  let insn_map = Map.set insn_map ~key:right ~data:(right_mem, None) in
  let rejoin = Addr.(right ++ incr) in
  let rejoin_mem = create_memory arch rejoin junk_data |> ok_exn in
  let insn_map = Map.set insn_map ~key:rejoin ~data:(rejoin_mem, None) in
  let insn_risg = add_edge insn_risg left branch_at in
  let insn_risg = add_edge insn_risg right branch_at in
  let insn_risg = add_edge insn_risg rejoin right in
  let insn_risg = add_edge insn_risg rejoin left in
  insn_map, insn_risg

let test_branch_recognition test_ctxt =
  let tail_addr = Addr.of_int ~width:addr_size 50 in
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg = 
    construct_branch insn_map insn_risg tail_addr 2 in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in
  let entries = Superset.entries_of_isg superset in
  let msg = "expect at least one entry" in
  assert_bool msg (Hash_set.(length entries) > 0);
  let branches = Grammar.identify_branches superset in
  let msg = sprintf 
      "expect branches to be detected! was %d"
      Hash_set.(length branches) in
  assert_bool msg (Hash_set.(length branches) = 1);
  let msg = "expect exact branch addr to be detected!" in
  assert_bool msg (Hash_set.(mem branches tail_addr));
  ()

let test_dfs_iter_order test_ctxt = 
  let insn_map, insn_risg = init () in
  let start = Addr.of_int ~width:addr_size 40 in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(start ++ 2) in
  let visit_order = ref [] in
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg arch in
  Traverse.with_ancestors_at 
    ~pre:(fun v -> visit_order := v :: !visit_order) 
    superset start;
  visit_order := List.rev !visit_order;
  let msg = sprintf "expected addr %s to be first, was %s" 
      Addr.(to_string start)
      (List.to_string ~f:Addr.to_string !visit_order) in
  match !visit_order with 
  | first :: _ -> 
    assert_equal ~msg first start
  | _ -> assert_bool msg false 

(* conflicts should include both the instruction at a data address *)
(* and the instruction whose body it is inside. *)
let test_find_all_conflicts test_ctxt =
  let insn_map, insn_risg = init () in
  let tail_addr = Addr.of_int ~width:addr_size 50 in
  let num_conflicts = 2 in
  let insn_map, insn_risg =
    construct_tail_conflict
      insn_map insn_risg tail_addr num_conflicts in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let msg = sprintf "expect %d conflicts" num_conflicts in
  assert_equal ~msg num_conflicts Set.(length conflicts)


(* Establish the idempotency or addition of edges. *)
let test_graph_edge_behavior test_ctxt =
  let _, insn_risg = init () in
  let start = Addr.of_int ~width:addr_size 50 in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let edges = Seq.filter (G.edges insn_risg) ~f:(fun e ->
                  Addr.((G.Edge.src e) = start)
                  && Addr.((G.Edge.dst e) = Addr.(succ start))) in
  let msg = "expect single edge between nodes" in
  assert_equal ~msg Seq.(length edges) 1

let test_streams test_ctxt =
  let strm, sgnl = Stream.create () in
  let called = ref false in
  Stream.watch strm (fun id _ ->
      called := true;
      Stream.unsubscribe strm id
    );
  Signal.send sgnl ();
  assert_bool "stream did not receive signal called" !called

let test_ssa test_ctxt =
  let arch = Arch.(`x86_64) in
  let make_chain bils =
    let bils = List.rev bils in
    let insn_map, insn_risg = init () in
    let lifted = Addr.Table.create () in
    let init = zero,insn_risg in
    let _,insn_risg =
      List.fold bils ~init ~f:(fun (accu,insn_risg) bil ->
          Addr.Table.set lifted ~key:accu ~data:bil;
          let s = Addr.succ accu in
          let e = G.Edge.create accu s () in
          s,G.Edge.insert e insn_risg
        ) in    
    Superset_impl.of_components ~insn_map ~insn_risg ~lifted arch in
  let find_ssa superset ~f = 
    let entries = Superset.entries_of_isg superset in
    assert_bool "Expect >= 1 entry in superset"
      ((Hash_set.length entries) > 0);
    let fssa = Liveness.compute_liveness superset in
    let ssa = Addr.Hash_set.create () in
    List.iter Set.(to_list fssa) ~f:Hash_set.(add ssa);
    f ssa in
  let superset =
    let open Bil in
    let v = Var.create "v" @@ Bil.Types.Imm 32 in
    let def = Bil.move v @@ Bil.Int zero in
    let use = Bil.move v ((Bil.Var v) + (Bil.Int (Addr.succ zero))) in
    make_chain [[def]; [use]] in
  find_ssa superset ~f:(fun ssa_rax ->
      assert_bool "Expect >= 1 ssa for move move"
        ((Hash_set.length ssa_rax) > 0));
  let superset =
    let open Bil in
    let s = Size.(`r64) in
    let mem = Bil.var @@ Var.create "mem" (Bil.Types.Imm 64) in
    let exp = Bil.var @@ Var.create "v1" (Bil.Types.Imm 64) in
    let addr = Var.create "addr" (Bil.Types.Imm 64) in
    let mv_addr = Bil.move addr Bil.(exp + (Int zero)) in
    let st = store ~mem ~addr:Bil.(Var addr) exp LittleEndian s; in
    let stv = Var.create "st" @@ Bil.Types.Imm 32 in
    let v2 = Var.create "v2" (Bil.Types.Imm 64) in
    let move_v2 = Bil.(move v2 Bil.(exp + (Int zero))) in
    let mem = Bil.var @@ Var.create "mem2" (Bil.Types.Imm 64) in
    let ld = load ~mem ~addr:(Var v2) LittleEndian s in
    let ldv = Var.create "ld" @@ Bil.Types.Imm 32 in
    let st = Bil.move stv st in
    let ld = Bil.move ldv ld in
    make_chain [[mv_addr]; [st]; [move_v2]; [ld];] in
  find_ssa superset ~f:(fun ssa_renamed ->
      let actual = (Hash_set.length ssa_renamed) in
      let msg =
        sprintf
          "Expect >= 2 ssa for recognition over renaming, was %d"
          actual in
      assert_bool msg (actual > 2));
  let superset =
    let open Bil in
    let s = Size.(`r64) in
    let mem = Bil.var @@ Var.create "mem" (Bil.Types.Imm 64) in
    let ld = load ~mem ~addr:(Int zero) LittleEndian s in
    let ldv = Var.create "ld" @@ Bil.Types.Imm 32 in
    let ld = [Bil.move ldv ld] in
    let exp = Bil.var @@ Var.create "v1" (Bil.Types.Imm 64) in
    let st = store ~mem ~addr:(Int zero) exp LittleEndian s; in
    let stv = Var.create "st" @@ Bil.Types.Imm 32 in
    let st = [Bil.move stv st] in
    make_chain [ld; st] in
  find_ssa superset ~f:(fun ssa_mem_mem ->
      assert_bool "Expect >= 1 ssa for load store, address exp equal"
        ((Hash_set.length ssa_mem_mem) > 0));
  let superset =
    let open Bil in
    let s = Size.(`r64) in
    let mem = Bil.var @@ Var.create "mem" (Bil.Types.Imm 64) in
    let ld = load ~mem ~addr:(Int zero) LittleEndian s in
    let ldv = Var.create "ld" @@ Bil.Types.Imm 32 in
    let ld = [Bil.move ldv ld] in
    let exp = Bil.var @@ Var.create "v1" (Bil.Types.Imm 64) in
    let st = store ~mem ~addr:(Int zero) exp LittleEndian s; in
    let stv = Var.create "st" @@ Bil.Types.Imm 32 in
    let st = [Bil.move stv st] in
    make_chain [ld; st] in
  find_ssa superset ~f:(fun ssa_mem_mem ->
      assert_bool
        "Expect >= 1 ssa for memory exp operation to same addr"
        ((Hash_set.length ssa_mem_mem) > 0));
  let superset =
    let open Bil in
    let s = Size.(`r64) in
    let mem = Bil.var @@ Var.create "x" (Bil.Types.Imm 64) in
    let ld = load ~mem ~addr:(Int zero) LittleEndian s in
    let ldv = Var.create "ld" @@ Bil.Types.Imm 32 in
    let ld = [Bil.move ldv ld] in
    let exp = Bil.((var ldv) + mem) in
    let st =
      store ~mem ~addr:(Int (Addr.succ zero)) exp LittleEndian s; in
    let stv = Var.create "st" @@ Bil.Types.Imm 32 in
    let st = [Bil.move stv st] in
    make_chain [ld; st] in
  find_ssa superset ~f:(fun ssa_load_store ->
      assert_bool
        "Expect >= 1 ssa for memory exp operation sharing variable"
        ((Hash_set.length ssa_load_store) > 0));
  let superset =
    let open Bil in
    let v1 = Var.create "v2" @@ Bil.Types.Imm 32 in
    let def1 = Bil.move v1 @@ Bil.Int zero in
    let v2 = Var.create "v2" @@ Bil.Types.Imm 32 in
    let def2 = Bil.move v2 @@ Bil.Var v1 in
    let v3 = Var.create "v3" @@ Bil.Types.Imm 32 in
    let use = Bil.move v3 ((Bil.Var v1) + (Bil.Var v2)) in
    let step1 = [def1] in
    let step2 = [def2] in
    let step3 = [use] in
    make_chain [step1; step2; step3; step3;] in
  find_ssa superset ~f:(fun ssa_chain ->
      assert_bool "Expect >= 2 ssa for register chain"
        ((Hash_set.length ssa_chain) > 1));
  ()

let () =
  let suite = 
    "suite">:::
    [
      "test_hits_every_byte" >:: test_hits_every_byte;
      "test_trim" >:: test_trim;
      "test_trims_invalid_jump" >:: test_trims_invalid_jump;
      "test_addr_map" >:: test_addr_map;
      "test_insn_isg" >:: test_insn_isg;
      "test_consistent_superset" >:: test_consistent_superset;
      "test_loop_scc" >:: test_loop_scc;
      "test_scc" >:: test_scc;
      "test_find_conflicts" >:: test_find_conflicts;
      "test_trim_scc" >:: test_trim_scc;
      "test_topological_revisit" >:: test_topological_revisit;
      "test_branch_recognition" >:: test_branch_recognition;
      "test_dfs_iter_order" >:: test_dfs_iter_order;
      "test_find_all_conflicts" >:: test_find_all_conflicts;
      "test_graph_edge_behavior" >:: test_graph_edge_behavior;
      "test_can_lift" >:: test_can_lift;
      "test_static_successors_includes_fall_through" >::
        test_static_successors_includes_fall_through;
      "test_brancher" >:: test_brancher;
      "test_lift" >:: test_lift;
      "test_successor_calculation" >:: test_successor_calculation;
      "test_superset_contains_addr" >:: test_superset_contains_addr;
      "test_target_not_in_mem" >:: test_target_not_in_mem;
      "test_tag_non_insn" >:: test_tag_non_insn;
      "test_tag_target_is_bad" >:: test_tag_target_is_bad;
      "test_target_in_body" >:: test_target_in_body;
      "test_streams" >:: test_streams;
      "test_ssa" >:: test_ssa;
    ] in
  run_test_tt_main suite
;;
