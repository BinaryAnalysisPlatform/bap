open Core_kernel.Std
open Or_error
open Bap_types.Std
open Bap_image_std
open Bap_disasm_target_factory
(*open Format*)

module Dis = Bap_disasm_basic
module Brancher = Bap_disasm_brancher

type elem = mem * (Dis.full_insn option)

type 'a t = {
  arch      : arch;
  img       : Image.t option;
  brancher  : Brancher.t;
  data      : 'a;
  insn_map  : (mem * (Dis.full_insn option)) Addr.Map.t;
  (* TODO: needs to become an array *)
  insn_risg : Superset_risg.t;
  bad       : Addr.Hash_set.t;
  keep      : Addr.Hash_set.t;
  (* TODO registerable per-feature info? *)
  (* marked data  *)
  (* visited *)
  (* union_find *)
} [@@deriving fields]

module type Superset_intf = sig
end

let contains_addr superset addr = 
  let img = Option.value_exn superset.img in
  let segments = Table.to_sequence Image.(segments img) in
  Seq.fold segments ~init:false ~f:(fun status (mem, segment) ->
      status || Memory.contains mem addr)

let get_img superset = Option.(value_exn superset.img)

let get_segments superset = 
  Image.segments Option.(value_exn superset.img)

let get_endianness superset = 
  Image.endian Option.(value_exn superset.img)

let get_arch superset = superset.arch

let get_graph superset = superset.insn_risg

let mark_bad superset addr =
  Hash_set.add superset.bad addr

let num_bad superset =
  Hash_set.length superset.bad

let clear_bad superset addr =
  Hash_set.remove superset.bad addr

let clear_all_bad superset =
  Hash_set.clear superset.bad

let with_bad superset ?visited ~pre ~post accu =
  Hash_set.fold ~init:accu superset.bad ~f:(fun accu b -> 
      if Superset_risg.G.mem_vertex superset.insn_risg b then
        Superset_risg.fold_component accu superset.insn_risg
          ?visited ~pre ~post b
      else accu
    )  

let get_map superset = superset.insn_map

let get_base superset =
  let insn_map = get_map superset in
  let (base_addr, _)  = Addr.Map.min_elt insn_map |> Option.value_exn in
  base_addr

let get_data superset = superset.data

let len_at superset at = 
  let insn_map = get_map superset in
  match Map.find insn_map at with
  | None -> 0
  | Some(mem, _) -> Memory.length mem

let fall_through_of superset addr =
  let len = len_at superset addr in
  Addr.(addr ++ len)

let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  (* TODO should check for edge *)
  Addr.(child = ft)

let get_callers superset addr =
  let g = (get_graph superset) in
  if Superset_risg.G.mem_vertex g addr &&
     Superset_risg.G.out_degree g addr > 0 then
    let callers = Superset_risg.G.succ g addr in
    List.filter callers ~f:(fun caller ->
        not (is_fall_through superset caller addr))
  else []

let get_non_fall_through_edges superset = 
  let g = (get_graph superset) in
  Superset_risg.G.fold_edges
    (fun child parent jmps -> 
       if is_fall_through superset parent child then
         Map.set jmps child parent
       else jmps
    ) g Addr.Map.empty

let get_callsites ?(threshold=6) superset =
  let g = (get_graph superset) in
  let callsites = Addr.Hash_set.create () in
  Superset_risg.G.iter_vertex
    (fun v -> 
       let callers = Superset_risg.G.succ g v in
       let num_callers = 
         List.fold callers ~init:0 ~f:(fun total caller -> 
             if not (is_fall_through superset caller v) then
               total + 1
             else total) in
       if num_callers > threshold then (
         Hash_set.add callsites v;
       )
    ) g;
  callsites

let with_data_of_insn superset at ~f =
  let len = len_at superset at in
  let body = Superset_risg.seq_of_addr_range at len in
  Seq.iter body ~f

let with_descendents_at ?insn_isg ?visited superset addr ~f =
  let insn_risg = get_graph superset in
  if Superset_risg.G.mem_vertex insn_risg addr then
    let insn_isg = 
      match insn_isg with 
      | Some insn_isg -> insn_isg 
      | None -> 
        Superset_risg.Oper.mirror insn_risg in
    Superset_risg.iter_component ?visited ~pre:f insn_isg addr

let mark_descendents_at ?insn_isg ?visited ?datas superset addr =
  let datas = Option.value datas 
      ~default:(Addr.Hash_set.create ()) in
  let mark_bad = mark_bad superset in
  with_descendents_at ?insn_isg ?visited superset addr
    ~f:(fun v ->
        with_data_of_insn superset v ~f:mark_bad;
        with_data_of_insn superset v ~f:(Hash_set.add datas);
      )

let create ?insn_map ?insn_risg arch data =
  let insn_map = Option.value insn_map ~default:Addr.Map.empty in
  let insn_risg = Option.value insn_risg 
      ~default:(Superset_risg.G.create ()) in
  {
    arch = arch;
    img = None;
    brancher = Brancher.of_bil arch;
    insn_map = insn_map;
    insn_risg = insn_risg;
    bad      = Addr.Hash_set.create ();
    keep      = Addr.Hash_set.create ();
    data = data;
  }

let rebuild ?data ?insn_map ?insn_risg superset =
  let insn_map = Option.value insn_map ~default:superset.insn_map in
  let data = Option.value data ~default:superset.data in
  let insn_risg = Option.value insn_risg ~default:superset.insn_risg in
  {
    arch      = superset.arch;
    brancher  = superset.brancher;
    img       = superset.img;
    bad       = superset.bad;
    data      = data;
    insn_risg = insn_risg;
    insn_map  = insn_map;
    keep      = superset.keep;
  }

let drop superset =
  rebuild ~data:() superset

let remove superset addr = 
  Superset_risg.G.remove_vertex superset.insn_risg addr;
  rebuild superset

let add_to_map superset mem insn = 
  let insn_map = get_map superset in
  let addr = (Memory.min_addr mem) in
  let insn_map = Addr.Map.set insn_map addr (mem, insn) in
  rebuild ~insn_map superset

let add_to_graph superset mem insn =
  let addr = Memory.min_addr mem in
  Superset_risg.G.add_vertex superset.insn_risg addr;
  rebuild superset

let add superset mem insn =
  let superset = add_to_graph superset mem insn in
  let superset = add_to_map superset mem insn in
  superset

let replace superset mem insn = 
  let addr = Memory.min_addr mem in
  let superset = remove superset addr in
  add superset mem insn

let format_cfg ?format superset =
  let format = Option.value format ~default:Format.std_formatter in
  Superset_risg.Gml.print format superset.insn_risg

let isg_to_string superset = 
  let format = Format.str_formatter in
  format_cfg ~format superset;
  Format.flush_str_formatter ()

let next_chunk mem ~addr =
  let next_addr = Addr.succ addr in
  Memory.view ~from:next_addr mem

let run_seq dis mem = 
  let open Seq.Generator in 
  let rec disasm cur_mem = 
    let elem = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, insn, _) -> (m, insn)
      | Error _ -> (cur_mem, None) in
    yield elem >>= fun () ->
    match next_chunk mem ~addr:(Memory.min_addr cur_mem) with
    | Ok next -> disasm next
    | Error _ -> return () in
  run (disasm mem)

let run dis ~accu ~f mem =
  Seq.fold ~init:accu ~f:(fun x y -> f y x) (run_seq dis mem)

let disasm ?(backend="llvm") ~accu ~f arch mem =
  Dis.with_disasm ~backend (Arch.to_string arch)
    ~f:(fun d -> Ok(run d ~accu ~f mem))

let lift_insn lift_fn (mem,insn) =
  let lift_fn = lift_fn mem in
  let insn = Option.map insn ~f:lift_fn in
  Option.map insn ~f:(fun bil -> (mem, bil |> ok_exn))

let lift arch insns =
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  let lifted_superset = Addr.Map.empty in
  List.fold insns ~init:lifted_superset
    ~f:(fun lifted_superset (mem, insn) -> 
        match lift_insn lifter (mem, insn) with
        | Some (mem, bil) -> 
          let addr = Memory.min_addr mem in 
          Map.set lifted_superset ~key:addr
            ~data:(bil, Memory.length mem)
        | None -> lifted_superset
      )

let memmap_all ?backend arch mem =
  let filter_add elem memmap =
    let (mem, insn) = elem in 
    Option.value_map insn ~default:memmap
      ~f:(Memmap.add memmap mem) in
  disasm ?backend ~accu:Memmap.empty ~f:filter_add arch mem |> ok_exn

let sexp_of_mem mem = 
  let endianness = Memory.endian mem in
  let maddr = Memory.min_addr mem in
  let bstr_mem = Memory.to_string mem in
  Tuple3.sexp_of_t 
    Addr.sexp_of_endian
    Addr.sexp_of_t
    String.sexp_of_t (endianness, maddr, bstr_mem)

let mem_of_sexp sexp_mem =
  let (endianness, maddr, mem) = 
    Tuple3.t_of_sexp
      Addr.endian_of_sexp
      Addr.t_of_sexp
      String.t_of_sexp sexp_mem in
  let mem = Bigstring.of_string mem in
  Memory.create endianness maddr mem |> ok_exn

let insn_map_to_string insn_map =
  Sexp.to_string @@ Addr.Map.sexp_of_t 
    (fun (mem, _) -> sexp_of_mem mem) insn_map

let insn_map_of_string map_str = 
  let map_sexp = Sexp.of_string map_str in
  Addr.Map.t_of_sexp (fun m -> mem_of_sexp m, None) map_sexp

let meta_of_string meta_str = 
  let sexp_meta = Sexp.of_string meta_str in
  Arch.t_of_sexp sexp_meta

let meta_to_string superset = 
  Sexp.to_string (Arch.sexp_of_t superset.arch)

let import bin =
  let insn_risg = Superset_risg.Gml.parse (bin ^ ".graph") in
  let map_str   = In_channel.read_all (bin ^ ".map") in
  let insn_map  = insn_map_of_string map_str in
  let meta_str  = In_channel.read_all (bin ^ ".meta") in
  let arch      = meta_of_string meta_str in
  let superset  = create ~insn_risg arch ~insn_map () in
  superset

let export bin superset = 
  let graph_f   = Out_channel.create (bin ^ ".graph") in
  let formatter = Format.formatter_of_out_channel graph_f in
  let () = Superset_risg.Gml.print formatter superset.insn_risg in
  let () = Out_channel.close graph_f in
  let insn_map = get_map superset in
  let map_str  = insn_map_to_string insn_map in
  Out_channel.write_all (bin ^ ".map") ~data:map_str;
  let meta_str  = meta_to_string superset in
  Out_channel.write_all (bin ^ ".meta") ~data:meta_str

let export_addrs bin superset =
  let insn_map = get_map superset in
  let addrs = Map.keys insn_map in
  let addrs = List.map addrs ~f:Addr.to_string in
  let base = Filename.basename bin in
  let addrs_file = Out_channel.create ("./" ^ base ^ "_addrs.txt") in
  Out_channel.output_lines addrs_file addrs

let update_with_mem ?backend ?f superset mem =
  let update = Option.value f ~default:(fun (m, i) a -> a) in
  let f (mem, insn) superset =
    let superset = add superset mem insn in
    update (mem, insn) superset in
  disasm ?backend ~accu:superset ~f superset.arch mem |> ok_exn

let with_img ~accu img ~f =
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        f ~accu mem
      else accu 
    )

let superset_of_img ~data ?f ~backend img =
  let arch = Image.arch img in
  let brancher = Brancher.of_bil arch in
  let superset = {
    data          = data;
    arch          = arch;
    bad           = Addr.Hash_set.create ();
    insn_risg     = Superset_risg.G.create ();
    insn_map      = Addr.Map.empty;
    brancher      = brancher;
    img           = Some img;
    keep          = Addr.Hash_set.create ();
  } in
  with_img ~accu:superset img
    ~f:(fun ~accu mem -> 
        update_with_mem ~backend accu mem ?f
      )

let superset_disasm_of_file ?(backend="llvm") ~data ?f binary = 
  let img  = Common.img_of_filename binary in
  let r = superset_of_img ~data ~backend img ?f in
  r

let fold_insns superset f =
  let insn_map = get_map superset in
  Addr.Map.fold ~init:superset insn_map ~f:(fun ~key ~data superset -> 
      let mem, insn = data in
      f ~superset ~mem ~insn
    )

let with_graph superset f =
  let insn_risg = superset.insn_risg in
  f insn_risg

let rebalance superset =
  let insn_map = get_map superset in
  let superset_risg = get_graph superset in
  Superset_risg.G.iter_vertex (fun vert ->
      if not Map.(mem insn_map vert) then (
        mark_bad superset vert;
      )
    ) superset_risg;
  let insn_map = Map.filteri ~f:(fun ~key ~data -> 
      let vert = key in
      (*let (mem, insn) = data in
        Option.is_some insn && *)
      Superset_risg.G.(mem_vertex superset_risg vert)
    ) insn_map in
  rebuild ~insn_risg:superset_risg ~insn_map superset
