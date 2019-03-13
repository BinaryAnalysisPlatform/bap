open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bil.Types
open Or_error
open Bap_image_std

module Targets = Bap_disasm_target_factory

module Dis = Bap_disasm_basic
module Brancher = Bap_disasm_brancher
module Rooter = Bap_disasm_rooter
module Addrs = Addr.Table
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn

type full_insn = Dis.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type block = Block.t
type edge = Block.edge [@@deriving compare, sexp]
type jump = Block.jump [@@deriving compare, sexp]
type lifter = Targets.lifter

type dis = (Dis.empty, Dis.empty) Dis.t

type dst = [
  | `Jump of addr option
  | `Cond of addr option
  | `Fall of addr
] [@@deriving sexp]

type error = [
  | `Failed_to_disasm of mem
  | `Failed_to_lift of mem * full_insn * Error.t
] [@@deriving sexp_of]

type maybe_insn = full_insn option * bil option [@@deriving sexp_of]
type decoded = mem * maybe_insn [@@deriving sexp_of]

type dests = Brancher.dests

module Node = struct
  type t = block
  include Opaque.Make(struct
      type nonrec t = t
      let addr = Block.addr
      let compare x y =
        Addr.compare (addr x) (addr y)
      let hash x = Addr.hash (addr x)
    end)
end

module Edge = struct
  type t = edge [@@deriving compare]
  include Opaque.Make(struct
      type nonrec t = t [@@deriving compare]
      let hash = Hashtbl.hash
    end)
end

module Cfg = Graphlib.Make(Node)(Edge)

type cfg = Cfg.t [@@deriving compare]

module Visited = struct
  type t = addr Addr.Map.t

  let empty = Addr.Map.empty
  let add t mem = Map.set t (Memory.min_addr mem) (Memory.max_addr mem)
  let find = Map.find
  let next t = Map.closest_key t `Greater_than
  let prev t = Map.closest_key t `Less_than
  let min = Map.min_elt
  let is_insn = Map.mem
  let mem t a =
    is_insn t a ||
    match prev t a with
    | None -> false
    | Some (addr,max_addr) -> Addr.(addr < a && a <= max_addr)
end

type blk_dest = [
  | `Block of block * edge
  | `Unresolved of jump
]

type stage1 = {
  base : mem;
  addr : addr;
  visited : Visited.t;
  roots : addr list;
  inits : Addr.Set.t;
  dests : dests Addr.Table.t;
  errors : (addr * error) list;
  lift : lifter;
  intersected : addr list Addr.Map.t;
}

type stage2 = {
  stage1 : stage1;
  addrs : mem Addrs.t;   (* table of blocks *)
  succs : dests Addrs.t;
  preds : addr list Addrs.t;
  disasm : mem -> decoded list;
}

type stage3 = {
  cfg : Cfg.t;
  failures : (addr * error) list;
}

type t = stage3

let errored s ty = {s with errors = (s.addr,ty) :: s.errors}

let update_dests s dests mem =
  let dests = List.map dests ~f:(fun d -> match d with
      | Some addr,_ when Memory.contains s.base addr -> d
      | _,kind -> None, kind) in
  let key = Memory.min_addr mem in
  match dests with
  | [] -> Addr.Table.add_exn s.dests ~key ~data:[]
  | dests -> List.iter dests ~f:(fun data ->
      Addr.Table.add_multi s.dests ~key ~data)

let collect ~dir ~stop_on ?cond s start =
  let add = match cond with
    | None -> fun x xs -> x :: xs
    | Some cond -> fun x xs -> if cond x then x :: xs else xs in
  let rec loop addr data =
    match dir s.visited addr with
    | None -> data
    | Some x when stop_on x -> data
    | Some x -> loop (fst x) (add x data) in
  loop start []

let exists_visited ~dir ~stop_on ~cond s start  =
  not (List.is_empty @@ collect ~dir ~stop_on ~cond s start)

let is_shared_max (_,a) (_,a') = Addr.equal a a'

let need_barrier s ((addr,max_addr) as insn) =
  let exists = exists_visited ~cond:(is_shared_max insn) s addr in
  exists ~dir:Visited.next ~stop_on:(fun (s,_) -> Addr.(s > max_addr)) ||
  exists ~dir:Visited.prev ~stop_on:(fun (_,f) -> Addr.(f < addr))

let find_intersections s =
  let update_barriers xs init =
    List.fold xs ~init ~f:(fun bs x ->
        if need_barrier s x then Set.add bs (fst x) else bs) in
  let find_intersected (addr, max_addr) =
    collect ~dir:Visited.next s addr
      ~stop_on:(fun (a,_) -> Addr.(a > max_addr)) in
  let next max_addr =
    let next_start = Addr.succ max_addr in
    match Visited.find s.visited next_start with
    | None -> Visited.next s.visited max_addr
    | Some max' -> Some (next_start, max') in
  let add data (addr,max) = Map.add_multi data max addr in
  let rec loop barriers data = function
    | None -> barriers,data
    | Some ((addr, max_addr) as insn) ->
      let insns = find_intersected insn in
      let barriers = update_barriers insns barriers in
      let barriers =
        if List.exists insns ~f:(is_shared_max insn)
        then Set.add barriers addr
        else barriers in
      let data = List.fold insns ~init:data ~f:add in
      loop barriers data (next max_addr) in
  loop Addr.Set.empty Addr.Map.empty (Visited.min s.visited)

let rec has_jump = function
  | [] -> false
  | Bil.Jmp _ :: _ | Bil.CpuExn _ :: _ -> true
  | Bil.If (_,y,n) :: xs -> has_jump y || has_jump n || has_jump xs
  | Bil.While (_,b) :: xs -> has_jump b || has_jump xs
  | _ :: xs -> has_jump xs

let ok_nil = function
  | Ok xs -> xs
  | Error _ -> []

let is_barrier s mem insn =
  Dis.Insn.is insn `May_affect_control_flow ||
  has_jump (ok_nil (s.lift mem insn))

let update s mem insn dests : stage1 =
  let s = { s with visited = Visited.add s.visited mem } in
  if is_barrier s mem insn then
    let () = update_dests s dests mem in
    let roots = List.(filter_map ~f:fst dests |> rev_append s.roots) in
    { s with roots }
  else {
    s with roots = Addr.succ (Memory.max_addr mem) :: s.roots
  }

(* switch to next root or finish if there're no roots *)
let next dis s =
  let rec loop s = match s.roots with
    | [] -> Dis.stop dis s
    | r :: roots when not(Memory.contains s.base r) ->
      loop {s with roots}
    | r :: roots when Visited.is_insn s.visited r ->
      loop {s with roots}
    | addr :: roots ->
      let mem = Memory.view ~from:addr s.base in
      let mem = Result.map_error mem ~f:(fun err -> Error.tag err "next_root") in
      mem >>= fun mem ->
      Dis.jump dis mem {s with roots; addr} in
  loop s

let stop_on = [`Valid]

(* find intersections, i.e. instructions which share bytes, but have different address,
   and update destinations for those of them, which also share max address to
   split a block in such place *)
let update_intersections disasm brancher s =
  let next dis s = match s.roots with
    | [] -> Dis.stop dis s
    | addr :: roots ->
      Memory.view ~from:addr s.base >>= fun mem ->
      Dis.jump dis mem {s with roots; addr} in
  let roots,intersected = find_intersections s in
  let s = {s with intersected; roots=Set.to_list roots} in
  match Set.min_elt roots with
  | None -> Ok s
  | Some addr ->
    Memory.view ~from:addr s.base >>= fun mem ->
    Dis.run disasm mem ~stop_on ~return ~init:s
      ~hit:(fun d mem insn s ->
          let () = update_dests s (brancher mem insn) mem in
          next d s)
      ~stopped:next

let stage1 ?(rooter=Rooter.empty) lift brancher disasm base =
  let roots =
    Rooter.roots rooter |> Seq.filter ~f:(Memory.contains base) in
  let addr,roots = match Seq.to_list roots with
    | r :: rs -> r,rs
    | [] -> Memory.min_addr base, [] in
  let init = {base; addr; visited = Visited.empty;
              intersected = Addr.Map.empty;
              roots; inits = Addr.Set.of_list roots;
              dests = Addrs.create (); errors = []; lift} in
  Memory.view ~from:addr base >>= fun mem ->
  Dis.run disasm mem ~stop_on ~return ~init
    ~hit:(fun d mem insn s ->
        next d (update s mem insn (brancher mem insn)))
    ~invalid:(fun d mem s -> next d (errored s (`Failed_to_disasm mem)))
    ~stopped:next >>= update_intersections disasm brancher

(* performs the initial markup.

   Returns three tables: leads, terms, and kinds. Leads is a mapping
   from leader to all predcessing terminators. Terms is a mapping from
   terminators to all successing leaders. Kinds is a mapping from a
   terminator addresses, to all outputs with each output including the
   kind annotation, e.g, Cond, Jump, etc. This also includes
   unresolved outputs.
*)

let sexp_of_addr addr =
  Sexp.Atom (Addr.string_of_value addr)

let create_indexes (dests : dests Addr.Table.t) =
  let leads = Addrs.create () in
  let terms = Addrs.create () in
  let succs = Addrs.create () in
  Addrs.iteri dests ~f:(fun ~key:src ~data:dests ->
      List.iter dests ~f:(fun dest ->
          Addrs.add_multi succs ~key:src ~data:dest;
          match dest with
          | None,_ -> ()
          | Some dst,_ ->
            Addrs.add_multi leads ~key:dst ~data:src;
            Addrs.add_multi terms ~key:src ~data:dst));
  leads, terms, succs

let join_destinations ?default dests =
  let jmp x = [ Bil.(jmp (int x)) ] in
  let undecided x =
    Bil.if_ (Bil.unknown "destination" (Type.Imm 1)) (jmp x) [] in
  let init = match default with
    | None -> []
    | Some x -> jmp x in
  Set.fold dests ~init ~f:(fun bil x -> undecided x :: bil)

let make_switch x dests =
  let case addr = Bil.(if_ (x = int addr) [jmp (int addr)] []) in
  let default = Bil.jmp x in
  Set.fold dests ~init:[default] ~f:(fun ds a -> case a :: ds)

let dests_of_bil bil =
  (object inherit [Addr.Set.t] Stmt.visitor
    method! visit_jmp e dests = match e with
      | Int w -> Set.add dests w
      | _ -> dests
  end)#run bil Addr.Set.empty

let add_destinations bil = function
  | [] -> bil
  | dests ->
    let d = dests_of_bil bil in
    let d' = Addr.Set.of_list dests in
    let n = Set.diff d' d in
    if Set.is_empty n then bil
    else
    if has_jump bil then
      (object inherit Stmt.mapper
        method! map_jmp = function
          | Int addr -> join_destinations ~default:addr n
          | indirect -> make_switch indirect n
      end)#run bil
    else bil @ join_destinations n

let disasm stage1 dis  =
  let dis = Dis.store_asm dis in
  let dis = Dis.store_kinds dis in
  fun mem ->
    Dis.run dis mem
      ~init:[] ~return:ident ~stopped:(fun s _ ->
          Dis.stop s (Dis.insns s)) |>
    List.map ~f:(function
        | mem, None -> mem,(None,None)
        | mem, (Some ins as insn) ->
          let dests =
            Addrs.find stage1.dests (Memory.min_addr mem) |>
            Option.value ~default:[] |>
            List.filter_map ~f:(function
                | a, (`Cond | `Jump) -> a
                | _ -> None) in
          let bil = match stage1.lift mem ins with
            | Ok bil -> bil
            | _ -> [] in
          mem, (insn, Some (add_destinations bil dests)))

let stage2 dis stage1 =
  let leads, terms, kinds = create_indexes stage1.dests in
  let addrs = Addrs.create () in
  let succs = Addrs.create () in
  let preds = Addrs.create () in
  let next = Addr.succ in
  let is_edge (addr,max_addr) =
    Addrs.mem leads (next max_addr) ||
    Addrs.mem kinds addr ||
    Addrs.mem stage1.dests addr ||
    Set.mem stage1.inits (next max_addr) in
  let is_insn = Visited.is_insn stage1.visited in
  let next_visited = Visited.next stage1.visited in
  let create_block start (addr,max_addr) =
    Memory.range stage1.base start max_addr >>= fun blk ->
    Addrs.add_exn addrs ~key:start ~data:blk;
    let () = match Addrs.find terms addr with
      | None -> ()
      | Some leaders -> List.iter leaders ~f:(fun leader ->
          Addrs.add_multi preds ~key:leader ~data:start) in
    let dests = match Addrs.find kinds addr with
      | Some dests -> dests
      | None when Addrs.mem leads (next max_addr) &&
                  not (Addrs.mem stage1.dests addr) ->
        Addrs.add_multi preds ~key:(next max_addr) ~data:start;
        [Some (next max_addr),`Fall]
      | None -> [] in
    Addrs.add_exn succs ~key:start ~data:dests;
    return () in
  let find_insn addr = Visited.find stage1.visited addr in
  let next_block start =
    let rec loop last start curr =
      match find_insn curr with
      | Some max_addr ->
        let insn = curr,max_addr in
        if is_edge insn then Some (start, insn)
        else loop (Some insn) start (next max_addr)
      | None -> match last with
        | Some last when is_insn start -> Some (start,last)
        | _ -> match next_visited curr with
          | Some (addr,_) -> loop None addr addr
          | None -> None in
    loop None start start in
  let next_unknown s =
    match next_block s with
    | None -> None
    | Some (s,_) when Hashtbl.mem addrs s -> None
    | x -> x in
  let fetch_blocks ?(find=next_block) start =
    let rec loop start =
      match find start with
      | None -> Ok ()
      | Some (start, ((_, max_addr) as insn)) ->
        create_block start insn >>= fun () ->
        loop (next max_addr) in
    loop start in
  let process_intersections () =
    Map.fold stage1.intersected ~init:[]
      ~f:(fun ~key:_ ~data:addrs init ->
          List.fold addrs ~init ~f:(fun acc addr ->
              fetch_blocks ~find:next_unknown addr :: acc)) |>
    Result.all_unit in
  match Visited.min stage1.visited with
  | None -> errorf "Provided memory doesn't contain a recognizable code"
  | Some (addr,_) ->
    fetch_blocks addr >>= fun () ->
    process_intersections () >>= fun () ->
    return {stage1; addrs; succs; preds; disasm = disasm stage1 dis}

let stage3 s2 =
  let is_found addr = Addrs.mem s2.addrs addr in
  let pred_is_found = is_found in
  let succ_is_found = function
    | None,_ -> true
    | Some addr,_ -> is_found addr in
  let filter bs ~f = Addrs.filter_map bs ~f:(fun ps ->
      match List.filter ps ~f with
      | [] -> None
      | preds -> Some preds) in
  let s2 = {
    s2 with
    succs = filter s2.succs ~f:succ_is_found;
    preds = filter s2.preds ~f:pred_is_found;
  } in
  let nodes = Addrs.create () in
  Addrs.iteri s2.addrs ~f:(fun ~key:addr ~data:mem ->
      s2.disasm mem |> List.filter_map ~f:(function
          | _,(None,_) -> None
          | mem,(Some insn,bil) ->
            Some (mem, Insn.of_basic ?bil insn)) |> function
      | [] -> ()
      | insns ->
        let node = Block.create mem insns in
        Addrs.set nodes ~key:addr ~data:node);
  let cfg =
    Addrs.fold nodes ~init:Cfg.empty ~f:(fun ~key:addr ~data:x cfg ->
        match Addrs.find s2.succs addr with
        | None -> Cfg.Node.insert x cfg
        | Some dests ->
          List.fold dests ~init:cfg ~f:(fun cfg dest -> match dest with
              | None,_ -> Cfg.Node.insert x cfg
              | Some d,e -> match Addrs.find nodes d with
                | None -> Cfg.Node.insert x cfg
                | Some y ->
                  let edge = Cfg.Edge.create x y e in
                  Cfg.Edge.insert edge cfg)) in
  return {cfg; failures = s2.stage1.errors}

let run ?(backend="llvm") ?brancher ?rooter arch mem =
  let b = Option.value brancher ~default:(Brancher.of_bil arch) in
  let brancher = Brancher.resolve b in
  let module Target = (val Targets.target_of_arch arch) in
  let lifter = Target.lift in
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun dis ->
      stage1 ?rooter lifter brancher dis mem >>= stage2 dis >>= stage3)

let cfg t = t.cfg
let errors s = List.map s.failures ~f:snd
