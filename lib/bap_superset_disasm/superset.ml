open Bap.Std
open Regular.Std
open Core
open Or_error
open Graphlib.Std
open Bap_knowledge
open Bap_core_theory

module Dis = Disasm_expert.Basic
open Superset_impl
type elem = Superset_impl.elem
type t = Superset_impl.t

(* private accessors *)
let get_graph superset = superset.insn_risg
let get_insns superset = superset.insns

(* private modifiers *)
let add_to_map superset mem insn = 
  let insns = get_insns superset in
  let addr = (Memory.min_addr mem) in
  Addr.Table.set insns ~key:addr ~data:(mem, insn);
  superset 

module OG = Graphlib.To_ocamlgraph(G)
  
let add_to_graph superset mem insn =
  let addr = Memory.min_addr mem in
  let insn_risg = G.Node.insert addr superset.insn_risg in
  { superset with insn_risg }

module Cache = struct
  let package = "superset-disasm"
  let sym_label =
    KB.Symbol.intern "superset" Theory.Program.cls
    
  let superset_graph_t =
    let sexp_of_edge (s,d) =
      Tuple2.sexp_of_t Addr.sexp_of_t Addr.sexp_of_t (s,d) in
    let equal (s1,d1) (s2,d2) = Addr.equal s1 s2 && Addr.equal d1 d2 in
    let inspect = List.sexp_of_t sexp_of_edge in
    Knowledge.Domain.optional
      ~inspect ~equal:(List.equal equal) "edges"

  let superset_graph_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = (addr * addr) list option [@@deriving bin_io] end)

  let superset_graph =
    let attr ty persistent name desc =
      let open Theory.Program in
      Knowledge.Class.property ~package cls name ty
        ~persistent
        ~public:true
        ~desc in
    attr superset_graph_t superset_graph_persistent "superset_graph"
      "Graph, including all edges and single nodes."
end
  
module Core = struct
  let add superset mem insn =
    let superset = add_to_graph superset mem insn in
    let superset = add_to_map superset mem insn in
    superset

  let remove superset addr =
    let insn_risg = OG.remove_vertex superset.insn_risg addr in
    Addr.Table.remove superset.insns addr;
    Addr.Table.remove superset.lifted addr;
    { superset with insn_risg; } 

  let empty arch =
    let brancher = Brancher.of_bil arch in
    let module Target = (val target_of_arch arch) in
    let lifter = Target.lift in
    {
      arch;
      filename = None;
      main_entry = None;
      sections = Memmap.empty;
      brancher;
      endianness= None;
      lifter;
      insns       = Addr.Table.create ();
      lifted      = Addr.Table.create ();
      insn_risg   = Graphlib.create (module G) ();
      bad         = Addr.Hash_set.create ();
      keep        = Addr.Hash_set.create ();
    }

  let lookup superset addr =
    Addr.Table.find superset.insns addr

  let mem superset addr =
    OG.mem_vertex superset.insn_risg addr

  let fold superset =
    Addr.Table.fold superset.insns

  let clear_bad superset addr =
    Hash_set.remove superset.bad addr

  let clear_each superset s =
    Hash_set.iter s ~f:(fun v -> 
        clear_bad superset v
      )
    
  let clear_all_bad superset =
    Hash_set.clear superset.bad

  let mark_bad superset addr =
    if OG.mem_vertex superset.insn_risg addr then
      Hash_set.add superset.bad addr

  let copy_bad superset =
    Hash_set.copy superset.bad
    
  let next_chunk mem ~addr =
    let next_addr = Addr.succ addr in
    Memory.view ~from:next_addr mem

  let seq_of_addr_range addr len = 
    let open Seq.Generator in
    let rec gen_next_addr cur_addr = 
      if Addr.(cur_addr >= (addr ++ len)) then
        return ()
      else
        yield cur_addr >>=  fun () -> 
        let next_addr = Addr.succ cur_addr in
        gen_next_addr next_addr
    in run (gen_next_addr Addr.(succ addr))
    
  (** Builds a sequence disassembly sequence at every byte offset of
  the memory mem. *)
  let run_seq dis mem =
    let start_addr = Memory.min_addr mem in
    let len = Memory.length mem in
    let addrs = seq_of_addr_range start_addr len in
    Seq.filter_map addrs ~f:(fun addr ->
        let m = Memory.view ~from:addr mem in
        match m with
        | Error _ -> None
        | Ok m -> (
            match Dis.insn_of_mem dis m with
            | Ok (m, insn, _) -> Some (m, insn)
            | Error _ -> Some (m, None)
          )
      )

  (** Fold over the memory at every byte offset with function f *)
  let run dis ~accu ~f mem =
    Seq.fold ~init:accu ~f:(fun x y -> f y x) (run_seq dis mem)

  (** This builds the disasm type, and runs it on the memory. *)
  let disasm ?(backend="llvm") ~addrs ~accu ~f arch memry =
    Or_error.map 
      (Dis.with_disasm ~backend (Arch.to_string arch)
        ~f:(fun d ->
          let rec next state (addrs,accu) =
            match Seq.next addrs with
            | None -> Dis.stop state (addrs,accu)
            | Some(addr,addrs) ->
               match Memory.view ~from:addr memry with
               | Error _ -> next state (addrs,accu)
               | Ok jtgt -> Dis.jump state jtgt (addrs,accu) in
          let invalid state m (addrs,accu) =
            let accu = f (m, None) accu in
            next state (addrs,accu) in
          let hit state m insn (addrs,accu) =
            let accu = f (m, (Some insn)) accu in 
            next state (addrs,accu) in
          Ok(Dis.run ~backlog:1 ~stop_on:[`Valid] ~invalid
               ~hit d ~init:(addrs,accu) ~return:Fn.id memry)
        )) ~f:(fun (_,accu) -> accu)

  let disasm_all ?(backend="llvm") ~accu ~f arch memry =
    let addrs = seq_of_addr_range
                  (Memory.min_addr memry) (Memory.length memry) in
    disasm ~backend ~addrs ~accu ~f arch memry 
    
  let lift_at superset addr =
    match Addr.Table.find superset.lifted addr with
    | Some (bil) -> Some (bil)
    | None -> (
      match lookup superset addr with
      | Some (mem, insn) -> (
        let bil =
          Option.value_map insn ~default:[] ~f:(fun insn ->
              match (superset.lifter mem insn) with
              | Ok bil ->
                 Addr.Table.set superset.lifted ~key:addr ~data:bil;
                 bil
              | _ -> []
            ) in
        Some (bil)
      )
      | None -> None
    )
            
  let lift_insn superset (mem,insn) =
    let addr = Memory.(min_addr mem) in
    lift_at superset addr
       
  (** Perform superset disassembly on mem and add the results. *)
  let update_with_mem ?backend ?addrs ?f superset mem =
    let f = Option.value f ~default:(fun (m, i) a -> a) in
    let f (mem, insn) superset =
      let superset = add superset mem insn in
      f (mem, insn) superset in
    let default = seq_of_addr_range
                    (Memory.min_addr mem) (Memory.length mem) in
    let addrs = Option.value addrs ~default  in
    disasm ?backend ~accu:superset ~f ~addrs superset.arch mem |> ok_exn

  let is_unbalanced superset =
    Addr.Table.length superset.insns <> OG.nb_vertex superset.insn_risg

  (** This function is required by the differences between propagating
  data removal maximally and maintaining associations between the
  address and the instruction decoded thereof. It strives to make the
  results of trimming and tagging consistent between the map and
  graph, in order to keep a consistent view. But this may not produce
  the one to one needed, since there are scenarios in which there can
  be mismatch in some scenarios that are possible in binaries. Using
  the tagging and other library together with fixpoint is sufficient
  to rule that out, however. *)
  let rebalance superset =
    let insns = get_insns superset in
    let insn_risg = get_graph superset in
    if is_unbalanced superset then
      OG.iter_vertex (fun vert ->
          if not Addr.Table.(mem insns vert)
             && Memmap.contains superset.sections vert then (
            mark_bad superset vert;
          )
        ) insn_risg;
    let insns = 
      if Hash_set.length superset.bad > 0 ||
           is_unbalanced superset then
        Addr.Table.filteri ~f:(fun ~key ~data -> 
            let vert = key in
            OG.(mem_vertex insn_risg vert)
          ) insns
      else superset.insns  in
    let lifted =
      if is_unbalanced superset then
        let f ~key ~data = OG.mem_vertex insn_risg key in
        Addr.Table.filteri superset.lifted ~f
      else superset.lifted in
    { superset with insn_risg; insns; lifted; }

end

  
module ISG = struct
  let ancestors superset =
    OG.succ superset.insn_risg

  let descendants superset =
    OG.pred superset.insn_risg

  let iter_vertex superset f =
    OG.iter_vertex f superset.insn_risg

  let fold_vertex superset f =
    OG.fold_vertex f superset.insn_risg

  let fold_edges superset f =
    let insn_risg = superset.insn_risg in 
    OG.fold_edges f insn_risg

  let to_list superset =
    let init = [] in
    let f s d acc = (s,d) :: acc in
    let l = fold_edges superset f init in
    let f v acc =
      let g = superset.insn_risg in 
      if OG.in_degree g v = 0 && OG.out_degree g v = 0 then
        (v,v) :: acc
      else acc in
    fold_vertex superset f l

  let link superset v1 v2 =
    let e = G.Edge.create v1 v2 () in
    let insn_risg = G.Edge.insert e superset.insn_risg in
    { superset with insn_risg }

  let unlink superset v1 v2 = 
    let insn_risg = OG.remove_edge superset.insn_risg v1 v2 in
    { superset with insn_risg }

  let mem_vertex superset = OG.mem_vertex superset.insn_risg

  let check_connected superset e1 e2 =
    match OG.find_all_edges
            superset.insn_risg e1 e2 with
    | [] -> false | _ -> true

  let raw_loops superset = 
    StrongComponents.scc_list superset.insn_risg

  let dfs_fold ?visited superset =
    fold_component ?visited superset.insn_risg

  let dfs ?(terminator=(fun _ -> true))
        ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ()) explore superset v =
    let visited = Option.value visited 
                    ~default:(Addr.Hash_set.create ()) in
    let rec visit v =
      Hash_set.add visited v;
      pre v;
      List.iter (explore superset v)
        ~f:(fun w ->
          if (not (Hash_set.mem visited w)) && (terminator w) then
            visit w) ;
      post v
    in if Core.mem superset v then visit v

  let fixpoint ?steps ?start ?rev ?step superset =
    Graphlib.fixpoint ?steps ?start ?rev ?step
      (module G) superset.insn_risg

  let print_dot ?colorings superset =
    (*if not (colorings = String.Map.empty) then*)
    let colorings = Option.value colorings ~default:String.Map.empty in
    let fout =
      Out_channel.create @@ Option.value_exn superset.filename
                            ^ ".dot" in
    let superset_isg = Oper.mirror superset.insn_risg in
    let insns = superset.insns in
    let module Layout =
      Make(struct
          let instance = (superset_isg, colorings, insns)
        end) in
    Layout.Dot.output_graph fout (superset_isg, colorings, insns)

  let format_isg ?format superset =
    let format = Option.value format ~default:Format.std_formatter in
    let format =
      fold_edges superset (fun src dst format ->
          Format.pp_print_string format @@ Addr.to_string src;
          Format.pp_print_string format @@ "-";
          Format.pp_print_string format @@ Addr.to_string dst;
          Format.pp_print_string format @@ "\n";
          format
        ) format in
    Format.pp_print_string format @@ "\n";
    iter_vertex superset (fun v ->
        Format.pp_print_string format @@ Addr.to_string v;
        Format.pp_print_string format @@ "\n";
      )

  let isg_to_string superset = 
    let format = Format.str_formatter in
    format_isg ~format superset;
    Format.flush_str_formatter ()

  let parse_isg bin =
    let str = In_channel.read_all bin in
    let gstr = String.split_lines str in (* heh *)
    let init = false,Graphlib.create (module G) ()  in
    let _,g = List.fold ~init gstr ~f:(fun (status,g) line ->
                  if not status then (
                    if String.equal line "" then true,g else (
                      let l = String.split line ~on:'-' in
                      let l = List.map l ~f:Addr.of_string in
                      match l with
                      | [src;dst] ->
                         let e = G.Edge.create src dst () in
                         status,G.Edge.insert e g
                      | _ -> status,g
                    )
                  ) else (
                    status, OG.add_vertex g @@ Addr.of_string line
                  )
      ) in g

  let filter superset subgraph =
    let insn_risg = superset.insn_risg in
    let g = Graphlib.create (module G) () in
    let g =
      Hash_set.fold subgraph ~init:g ~f:(fun g addr ->
          let g = OG.add_vertex g addr in
          let g = OG.fold_succ
                    (fun s g ->
                      if Hash_set.mem subgraph s then
                        OG.add_edge g addr s
                      else g
                    ) insn_risg addr g in
          let g = OG.fold_pred
                    (fun s g ->
                      if Hash_set.mem subgraph s then
                        OG.add_edge g s addr
                      else g
                    ) insn_risg addr g in g
        ) in 
    { superset with insn_risg =g; }

end

module Inspection = struct

  let contains_addr superset addr =
    Memmap.contains superset.sections addr

  let total_bytes superset =
    Seq.fold (Memmap.to_sequence superset.sections) ~init:0
      ~f:(fun total (mem,_) -> (total + (Memory.length mem)))

  let count superset = OG.nb_vertex superset.insn_risg

  let count_unbalanced superset = Addr.Table.length superset.insns

  let unbalanced_diff superset =
    let kys = (Addr.Table.keys superset.insns) in
    let mapaddrs =
      List.fold kys ~init:Addr.Set.empty ~f:Set.add in
    let gaddrs = OG.fold_vertex (fun x s -> Set.add s x)
        superset.insn_risg Addr.Set.empty in
    Set.diff mapaddrs gaddrs, Set.diff gaddrs mapaddrs

  let get_memmap superset = superset.sections

  let get_main_entry superset = superset.main_entry

  let filename superset = superset.filename

  let static_successors superset mem insn =
    let brancher = superset.brancher in
    let addr = Memory.min_addr mem in
    if Addr.Table.mem superset.lifted addr then
      let l = ISG.descendants superset addr in
      List.map l ~f:(fun dst -> (Some dst, `Fall))
    else
      match insn with 
      | None -> [None, `Fall]
      | Some insn -> 
         try
           Brancher.resolve brancher mem insn
         with _ -> (
           print_endline @@ 
             "Target resolve failed on memory at " ^ Memory.to_string mem; 
           [None, `Fall] (*KB.return []*)
         )

  let len_at superset at = 
    let insns = get_insns superset in
    match Addr.Table.find insns at with
    | None -> 0
    | Some(mem, _) -> Memory.length mem

  let num_bad superset =
    Hash_set.length superset.bad

  let is_bad_at superset at = Hash_set.mem superset.bad at

  let get_segments superset = superset.sections

  let get_endianness superset = superset.endianness

  let get_arch superset = superset.arch

end

module Occlusion = struct

  let range_seq_of_conflicts ~mem addr len = 
    let range_seq = Core.seq_of_addr_range addr len in
    Seq.filter range_seq ~f:mem

  let conflict_seq_at superset addr =
    let insns = superset.insns in
    let check_mem = Addr.Table.(mem insns) in
    match Addr.Table.find insns addr with
    | Some(mem, _) -> 
      let len = Memory.length mem  in
      range_seq_of_conflicts ~mem:check_mem addr len
    | None -> Seq.empty

  let with_data_of_insn superset at ~f =
    let len = Inspection.len_at superset at in
    let body = Core.seq_of_addr_range at len in
    Seq.iter body ~f

  let conflicts_within_insn_at superset ?mem ?conflicts addr =
    let default = (OG.mem_vertex superset.insn_risg) in
    let mem = Option.value mem ~default in
    let conflicts = Option.value conflicts ~default:Addr.Set.empty in
    match Core.lookup superset addr with
    | Some ((m, _)) ->
       let len = (Memory.length m) in
       let rng = range_seq_of_conflicts ~mem addr len in
       let conflicts =
         if not (Seq.is_empty rng) then
           Set.add conflicts addr
         else conflicts in
       Seq.fold rng ~init:conflicts ~f:Set.add
    | None -> conflicts

  let find_all_conflicts ?mem superset =
    let insns = superset.insns in
    List.fold Addr.Table.(keys insns) ~init:Addr.Set.empty
      ~f:(fun conflicts addr -> 
          conflicts_within_insn_at superset ?mem ~conflicts addr
        )

  let conflicts_within_insns superset keep =
    Set.fold keep ~init:Addr.Set.empty
      ~f:(fun conflicts addr -> 
          conflicts_within_insn_at superset
            ~conflicts addr
        )

  (* It is possible that scenarios admit an instruction that is not
     the parent of a shared child that contains this addr *)
  let parent_conflict_at superset addr =
    let children = ISG.descendants superset addr in
    List.fold children ~init:Addr.Set.empty ~f:(fun cparents child -> 
        let parents = ISG.ancestors superset child in
        List.fold parents ~init:cparents ~f:(fun cparents parent -> 
            if not Addr.(parent = addr) then
              match Core.lookup superset parent with
              | Some(mem, _) -> 
                 let len = Memory.length mem in
                 if Addr.(parent < addr) && Addr.(addr < (parent ++ len)) then
                   Set.add cparents parent
                 else cparents
              | None -> cparents
            else cparents
          )
      )
    
end

(** An address is an entry for an isg if it could be the return
 ** terminating instruction at the end of a function. *)
let is_entry superset addr =
  let insn_isg = superset.insn_risg in
  OG.in_degree insn_isg addr  = 0 &&
  OG.out_degree insn_isg addr > 0

let entries_of_isg superset =
  let insn_isg = superset.insn_risg in
  OG.fold_vertex (fun addr accu ->
      if is_entry superset addr then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_isg (Addr.Hash_set.create ())

(** A frond point is a point that is distant most of a terminating
 ** instruction, meaning it may be the first instruction of a
 ** function. However, these could actually occur either within or
 ** beyond the body of instruction sequence intended.  *)
let is_frond_point superset addr =
  let insn_isg = superset.insn_risg in
  OG.in_degree insn_isg addr  > 0 &&
  OG.out_degree insn_isg addr = 0

let frond_of_isg superset =
  let insn_isg = superset.insn_risg in
  OG.fold_vertex (fun addr accu ->
      if is_frond_point superset addr then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_isg (Addr.Hash_set.create ())
  
let mergers superset =
  let insn_risg= superset.insn_risg in
  OG.fold_vertex (fun addr mergers ->
      if OG.out_degree insn_risg addr > 1 then
        Addr.Set.add mergers addr
      else mergers) insn_risg Addr.Set.empty

let is_branch superset addr =
  OG.in_degree superset.insn_risg addr >= 2

let get_branches superset =
  let branches = Addr.Hash_set.create () in
  ISG.iter_vertex superset (fun vert -> 
      if is_branch superset vert then
        Hash_set.add branches vert;
    );
  branches

(* This is a traversal
   val with_bad :
   t ->
   ?visited:'b Addr.Hash_set.t_ ->
   pre:('c -> addr -> 'd) ->
   post:('d -> addr -> 'c) -> 'c -> 'c
*)
let with_bad superset ?visited ~pre ~post accu =
  let visited =
    match visited with
    | None -> (Addr.Hash_set.create ())
    | Some visited -> visited in
  Hash_set.fold ~init:accu superset.bad ~f:(fun accu b -> 
      if OG.mem_vertex superset.insn_risg b then
        ISG.dfs_fold superset ~visited
          ~pre ~post accu b
      else accu
    )  

let fall_through_of superset addr =
  let len = Inspection.len_at superset addr in
  Addr.(addr ++ len)

let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  Addr.(child = ft)

let get_non_fall_through_edges superset = 
  ISG.fold_edges superset
    (fun child parent jmps -> 
       if is_fall_through superset parent child then
         Map.set jmps ~key:child ~data:parent
       else jmps
    ) Addr.Map.empty
  
let get_callers superset addr =
  let g = (get_graph superset) in
  if OG.mem_vertex g addr &&
     OG.out_degree g addr > 0 then
    let callers = OG.succ g addr in
    List.filter callers ~f:(fun caller ->
        not (is_fall_through superset caller addr))
  else []
  
let with_img ~accu img ~f =
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        f ~accu mem
      else accu 
    )
  
let superset_of_img ?f ?addrs ~backend img =
  let arch = Image.arch img in
  let segments =   Image.memory img in
  let main_entry = Image.entry_point img in
  let filename = Image.filename img in
  let f = Option.value f ~default:(fun (m, i) a -> a) in
  let superset =
    of_components ~main_entry ?filename ~segments arch in
  with_img ~accu:superset img
    ~f:(fun ~accu mem ->
      Core.update_with_mem ~backend ?addrs accu mem ~f
    )

let superset_disasm_of_file ?(backend="llvm") ?f ?addrs binary =
  let img, errs = Image.create ~backend binary |> ok_exn in
  superset_of_img ~backend img ?addrs ?f

