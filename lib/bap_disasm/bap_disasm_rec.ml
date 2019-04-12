open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bil.Types
open Or_error
open Bap_core_theory
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

type code = Theory.Program.t [@@deriving bin_io, compare, sexp]
type maybe_insn = full_insn option * code [@@deriving sexp_of]
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


(** Interval tree spanning visited memory *)
module type Span = sig
  type range = addr * addr
  type t [@@deriving sexp_of]

  val empty : t
  val add : t -> range -> t
  val mem : t -> addr -> bool
  val upper_bound : t -> addr -> addr option
  val min : t -> addr option
  val max : t -> addr option
  val pp : Format.formatter -> t -> unit
end

module Span : Span = struct
  module Range = struct
    module T = struct
      type t = addr * addr [@@deriving sexp]
      let compare (a1,_) (a2,_) = Addr.compare a1 a2
    end
    type t = T.t [@@deriving sexp]
    include Comparable.Make(T)
  end

  type range = Range.t

  let sexp_of_range (a0,a1) =
    Sexp.List [
      Sexp.Atom (Addr.string_of_value a0);
      Sexp.Atom (Addr.string_of_value a1);
    ]

  type t =
    | Empty
    | Node of t * range * addr * t
  [@@deriving sexp_of]

  let rec pp fmt = function
    | Empty -> ()
    | Node (lhs,(x,y),_,rhs) ->
      Format.fprintf fmt "@[%a(%a,%a)@,%a@]"
        pp lhs Addr.pp x Addr.pp y pp rhs

  let empty = Empty

  let rng_max (_,a1) a2 = Addr.max a1 a2

  (** @pre: x <= p *)
  let intersects (_,y) (p,_) = p <= y
  (** [merge x y] if [intersects x y]  *)
  let merge (x,p) (_,q) : range = (x, Addr.max p q)

  let rec add t ((_,a1) as rng) = match t with
    | Empty -> Node (Empty,rng,a1,Empty)
    | Node (lhs,top,m,rhs) ->
      let m = rng_max rng m in
      if Range.(rng < top) then
        if intersects rng top
        then Node (lhs, merge rng top, m, rhs)
        else Node (add lhs rng, top, m, rhs)
      else if intersects top rng
      then Node (lhs, merge top rng, m, rhs)
      else Node (lhs, top, m, add rhs rng)

  let rec mem t addr = match t with
    | Empty -> false
    | Node (lhs,(a0,a1),mx,rhs) ->
      if Addr.(addr > mx) then false
      else if Addr.(addr < a0) then mem lhs addr
      else if Addr.(addr >= a1) then mem rhs addr
      else true

  (** [upper_bound visited addr] returns minimum address that is
      greater than [addr] and is visited. Returns [None] if no such
      address was found *)
  (* returns the lowest left bound greater than [addr] *)
  let rec upper_bound t a = match t with
    | Empty -> None
    | Node (_,_,mx,_) when Addr.(a > mx) -> None
    | Node (lhs,(a0,_),_,rhs) ->
      if Addr.(a > a0) then upper_bound rhs a
      else match upper_bound lhs a with
        | None -> Some a0
        | some -> some

  let rec min = function
    | Empty -> None
    | Node (Empty,(a0,_),_,_) -> Some a0
    | Node (lhs,_,_,_) -> min lhs

  let max = function
    | Empty -> None
    | Node (_,_,m,_) -> Some m
end



type blk_dest = [
  | `Block of block * edge
  | `Unresolved of jump
]



type stage1 = {
  base : mem;
  addr : addr;
  visited : Span.t;
  valid : Addr.Set.t;
  roots : addr list;
  inits : Addr.Set.t;
  dests : dests Addr.Table.t;
  errors : (addr * error) list;
  lift : mem -> full_insn -> code Or_error.t;
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
  let key = Memory.max_addr mem in
  match dests with
  | [] -> Addr.Table.add_exn s.dests ~key ~data:[]
  | dests -> List.iter dests ~f:(fun data ->
      Addr.Table.add_multi s.dests ~key ~data)

let rec has_jump = function
  | [] -> false
  | Bil.Jmp _ :: _ | Bil.CpuExn _ :: _ -> true
  | Bil.If (_,y,n) :: xs -> has_jump y || has_jump n || has_jump xs
  | Bil.While (_,b) :: xs -> has_jump b || has_jump xs
  | _ :: xs -> has_jump xs

let ok_nil = function
  | Ok xs -> xs
  | Error _ -> []

let to_bil s = match s with
  | Error err -> Error err
  | Ok s ->
    Result.return @@
    KB.Value.get Bil.slot @@
    KB.Value.get Theory.Program.Semantics.slot s

let with_bil : code -> bil -> code = fun code bil ->
  let slot = Theory.Program.Semantics.slot in
  KB.Value.put Theory.Program.Semantics.slot code @@
  KB.Value.put Bil.slot (KB.Value.get slot code) bil

let is_barrier s mem insn =
  Dis.Insn.is insn `May_affect_control_flow ||
  has_jump (ok_nil (to_bil (s.lift mem insn)))

let update s mem insn dests : stage1 =
  let s = {s with valid = Set.add s.valid (Memory.min_addr mem)} in
  if is_barrier s mem insn then
    let dests = List.map dests ~f:(fun d -> match d with
        | Some addr,_ when Memory.contains s.base addr -> d
        | _,kind -> None, kind) in
    update_dests s dests mem;
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
    | r :: roots when Span.mem s.visited r ->
      loop {s with roots}
    | addr :: roots ->
      let mem = match Span.upper_bound s.visited addr with
        | None -> Memory.view ~from:addr s.base
        | Some r1 -> Memory.range s.base addr r1  in
      let mem =
        Result.map_error mem ~f:(fun err -> Error.tag err "next_root") in
      mem >>= fun mem -> Dis.jump dis mem {s with roots; addr} in
  let visited = Span.add s.visited (s.addr, Dis.addr dis) in
  loop {s with visited}

let stop_on = [`Valid]

let stage1 ?(rooter=Rooter.empty) lift brancher disasm base =
  let roots =
    Rooter.roots rooter |> Seq.filter ~f:(Memory.contains base) in
  let addr,roots = match Seq.to_list roots with
    | r :: rs -> r,rs
    | [] -> Memory.min_addr base, [] in
  let init = {base; addr; visited = Span.empty;
              roots; inits = Addr.Set.of_list roots; valid = Addr.Set.empty;
              dests = Addr.Table.create (); errors = []; lift} in
  Memory.view ~from:addr base >>= fun mem ->
  Dis.run disasm mem ~stop_on ~return ~init
    ~hit:(fun d mem insn s -> next d (update s mem insn (brancher mem insn)))
    ~invalid:(fun d mem s -> next d (errored s (`Failed_to_disasm mem)))
    ~stopped:next

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
  Addr.Table.iteri dests ~f:(fun ~key:src ~data:dests ->
      List.iter dests ~f:(fun dest ->
          Addrs.add_multi succs ~key:src ~data:dest;
          match dest with
          | None,_ -> ()
          | Some dst,_ ->
            Addrs.add_multi leads ~key:dst ~data:src;
            Addrs.add_multi terms ~key:src ~data:dst));
  leads, terms, succs

let filter_valid s = {s with inits = Set.inter s.inits s.valid}

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

let stage2 dis stage1 =
  let stage1 = filter_valid stage1 in
  let leads, terms, kinds = create_indexes stage1.dests in
  let addrs = Addrs.create () in
  let succs = Addrs.create () in
  let preds = Addrs.create () in
  let next = Addr.succ in
  let is_edge addr =
    Addrs.mem leads (next addr) ||
    Addrs.mem kinds addr ||
    Addrs.mem stage1.dests addr ||
    Addr.Set.mem stage1.inits (next addr) in
  let is_visited = Span.mem stage1.visited in
  let next_visited = Span.upper_bound stage1.visited in
  let create_block start finish =
    Memory.range stage1.base start finish >>= fun blk ->
    Addrs.add_exn addrs ~key:start ~data:blk;
    let () = match Addrs.find terms finish with
      | None -> ()
      | Some leaders -> List.iter leaders ~f:(fun leader ->
          Addrs.add_multi preds ~key:leader ~data:start) in
    let dests = match Addrs.find kinds finish with
      | Some dests -> dests
      | None when Addrs.mem leads (next finish) &&
                  not (Addrs.mem stage1.dests finish) ->
        Addrs.add_multi preds ~key:(next finish) ~data:start;
        [Some (next finish),`Fall]
      | None -> [] in
    Addrs.add_exn succs ~key:start ~data:dests;
    return () in

  let rec loop start curr' =
    let curr = next curr' in
    if is_visited curr then
      if is_edge curr then
        create_block start curr >>= fun () ->
        loop (next curr) curr
      else loop start curr
    else match next_visited curr with
      | Some addr -> loop addr addr
      | None when is_visited start && is_visited curr' ->
        create_block start curr'
      | None -> return () in
  match Span.min stage1.visited with
  | None -> errorf "Provided memory doesn't contain a recognizable code"
  | Some addr -> loop addr addr >>= fun () ->
    let dis = Dis.store_asm dis in
    let dis = Dis.store_kinds dis in
    let empty = Theory.Program.empty in
    let disasm mem =
      Dis.run dis mem
        ~init:[] ~return:ident ~stopped:(fun s _ ->
            Dis.stop s (Dis.insns s)) |>
      List.map ~f:(function
          | mem, None -> mem,(None,empty)
          | mem, (Some ins as insn) ->
            let dests =
              Addrs.find stage1.dests (Memory.max_addr mem) |>
              Option.value ~default:[] |>
              List.filter_map ~f:(function
                  | a, (`Cond | `Jump) -> a
                  | _ -> None) in
            let bil,sema = match stage1.lift mem ins with
              | Ok sema -> Insn.bil sema,sema
              | _ -> [],Theory.Program.empty in
            let bil = add_destinations bil dests in
            mem, (insn, with_bil sema bil)) in
    return {stage1; addrs; succs; preds; disasm}

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
          | mem,(Some _,sema) -> Some (mem, sema)) |> function
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



let lifter arch mem insn =
  let open KB.Syntax in
  let code =
    KB.Object.create Theory.Program.cls >>= fun code ->
    KB.provide Arch.slot code (Some arch) >>= fun () ->
    KB.provide Memory.slot code (Some mem) >>= fun () ->
    KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
    code in
  match Bap_state.run Theory.Program.cls code with
  | Ok prog ->
    let bil = Insn.bil prog in
    let prog' = Insn.of_basic ~bil insn in
    Ok (KB.Value.merge ~on_conflict:`drop_right prog prog')
  | Error _ ->
    Ok (KB.Value.empty Theory.Program.cls)

let run ?(backend="llvm") ?brancher ?rooter arch mem =
  let b = Option.value brancher ~default:(Brancher.of_bil arch) in
  let brancher = Brancher.resolve b in
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun dis ->
      stage1 ?rooter (lifter arch) brancher dis mem >>= stage2 dis >>= stage3)

let cfg t = t.cfg
let errors s = List.map s.failures ~f:snd
