open Core_kernel
open Bap_types.Std
open Bap_core_theory
open Bap_image_std

module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn

type full_insn = Dis.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type edge = [`Jump | `Cond | `Fall]

module Machine : sig
  type dsts = (addr option * [`Jump | `Cond | `Fall]) list
  type task = private
    | Dest of {dst : addr; parent : task option}
    | Fall of {dst : addr; parent : task}
    | Jump of {src : addr; age: int; dsts : dsts; parent : task}

  type state = private {
    stop : bool;
    work : task list;           (* work list*)
    curr : task;
    addr : addr;                (* current address *)
    begs : Set.M(Addr).t;       (* begins of basic blocks *)
    jmps : dsts Map.M(Addr).t;  (* jumps  *)
    code : Set.M(Addr).t;       (* all valid instructions *)
    data : Set.M(Addr).t;       (* all non-instructions *)
    usat : Set.M(Addr).t;       (* unsatisfied constraints *)
  }

  val start :
    ?is_code:(addr -> bool option) ->
    mem -> Set.M(Addr).t -> f:(state -> mem -> state) -> state
  val view : state -> mem -> f:(state -> mem -> state) -> state

  val failed : state -> addr -> state
  val jumped : state -> mem -> dsts -> int -> state
  val stopped : state -> state
  val moved  : state -> mem -> state
  val is_ready : state -> bool
end = struct
  type dsts = (addr option * [`Jump | `Cond | `Fall]) list

  type task =
    | Dest of {dst : addr; parent : task option}
    | Fall of {dst : addr; parent : task}
    | Jump of {src : addr; age: int; dsts : dsts; parent : task}

  let init_work roots =
    Set.to_sequence ~order:`Decreasing roots |>
    Seq.fold ~init:[] ~f:(fun work root ->
        Dest {dst=root; parent=None} :: work)

  type state = {
    stop : bool;
    work : task list;           (* work list*)
    curr : task;
    addr : addr;                (* current address *)
    begs : Set.M(Addr).t;       (* begins of basic blocks *)
    jmps : dsts Map.M(Addr).t;  (* jumps  *)
    code : Set.M(Addr).t;       (* all valid instructions *)
    data : Set.M(Addr).t;       (* all non-instructions *)
    usat : Set.M(Addr).t;       (* unsatisfied constraints *)
  }

  let is_code s addr = Set.mem s.code addr
  let is_data s addr = Set.mem s.data addr
  let is_visited s addr = is_code s addr || is_data s addr
  let is_ready s = s.stop


  let mark_data s addr = {
    s with
    begs = Set.remove s.begs addr;
    jmps = Map.remove s.jmps addr;
    usat = Set.remove s.usat addr;
    data = Set.add s.data addr;
  }

  let count_valid s dsts =
    List.count dsts ~f:(function
        | Some dst,_ -> not (Set.mem s.data dst)
        | None,_ -> true)

  let rec cancel task s = match task with
    | Dest {parent=None} -> s
    | Dest {parent=Some parent} | Fall {parent} | Jump {parent} ->
      match parent with
      | Dest {dst; parent = Some parent}
      | Fall {dst; parent} -> cancel parent (mark_data s dst)
      | Dest {dst; parent = None} -> mark_data s dst
      | Jump {src; parent; dsts} -> match task with
        | Fall _ | Jump _ -> s
        | Dest _ ->
          if count_valid s dsts = 0
          then cancel parent (mark_data s src)
          else s

  let rec step s = match s.work with
    | [] ->
      if Set.is_empty s.usat then {s with stop = true}
      else step {s with work = init_work s.usat}
    | Dest {dst=next} as curr :: work ->
      let s = if is_data s next
        then step @@ cancel curr {s with work}
        else {s with begs = Set.add s.begs next} in
      if is_visited s next then step {s with work}
      else {s with work; addr=next; curr}
    | Fall {dst=next} as curr :: work ->
      if is_code s next
      then step {s with begs = Set.add s.begs next; work}
      else if is_data s next then step @@ cancel curr {s with work}
      else {s with work; addr=next; curr}
    | (Jump {src; dsts} as jump) :: ([] as work)
    | (Jump {src; dsts; age=0} as jump) :: work ->
      let init = {s with jmps = Map.add_exn s.jmps src dsts; work} in
      step @@
      List.fold dsts ~init ~f:(fun s -> function
          | Some next,(`Jump|`Cond) when not (is_visited s next) ->
            {s with work = Dest {dst=next; parent = Some jump} :: s.work}
          | _ -> s)
    | Jump jmp :: (Fall {dst=next} as fall) :: work -> step {
        s with
        work = fall :: Jump {
            jmp with age = jmp.age-1; src = next;
          } :: work
      }
    | Jump jmp :: work -> step {
        s with work = Jump {jmp with age=0} :: work
      }

  let decoded s mem =
    let addr = (Memory.min_addr mem) in {
      s with code = Set.add s.code addr;
             usat = Set.remove s.usat addr
    }

  let jumped s mem dsts delay =
    let s = decoded s mem in
    let parent = s.curr in
    let src = Memory.min_addr mem in
    let jump = Jump {src; age=delay; dsts; parent} in
    let next = Addr.succ (Memory.max_addr mem) in
    let next = Fall {dst=next; parent=jump} in
    step {s with work = jump :: next :: s.work }

  let moved s mem =
    step @@ decoded {
      s with work = Fall {
        dst = Addr.succ (Memory.max_addr mem);
        parent = s.curr;
      } :: s.work;
    } mem


  let failed s addr =
    let s = mark_data s addr in
    let work = if is_visited s (Addr.succ addr) then s.work
      else Dest {dst=Addr.succ addr; parent=None} :: s.work in
    step {s with work}

  let stopped s = step @@ mark_data s s.addr

  let rec view s base ~f = match Memory.view ~from:s.addr base with
    | Ok mem -> f s mem
    | Error _ -> match s.work with
      | [] -> step s
      | _  -> view (step s) base ~f

  let start ?is_code mem roots =
    let empty = Set.empty (module Addr) in
    let usat,data = match is_code with
      | None -> empty,empty
      | Some is_code ->
        let base = Memory.min_addr mem in
        Seq.range 0 (Memory.length mem) |>
        Seq.fold ~init:(empty,empty) ~f:(fun (code,data) off ->
            let addr = Addr.(nsucc base off) in
            match is_code addr with
            | None -> code,data
            | Some false -> code,Set.add data addr
            | Some true -> Set.add code addr,data) in
    let work = init_work roots in
    let start = match Set.min_elt roots with
      | Some start -> start
      | None -> Memory.min_addr mem in
    view {
      work; data; usat;
      addr = start;
      curr = Dest {dst = start; parent = None};
      stop = false;
      begs = Set.empty (module Addr);
      jmps = Map.empty (module Addr);
      code = Set.empty (module Addr);
    } mem
end


let rec has_jump = function
  | [] -> false
  | Bil.Jmp _ :: _ | Bil.CpuExn _ :: _ -> true
  | Bil.If (_,y,n) :: xs -> has_jump y || has_jump n || has_jump xs
  | Bil.While (_,b) :: xs -> has_jump b || has_jump xs
  | _ :: xs -> has_jump xs


let delay insn = match KB.Value.get Insn.Slot.delay insn with
  | None -> 0
  | Some x -> x

let stop_on = [`Valid]

let dests _ = []

let scan_mem ?is_code (roots : Addr.Set.t) lift disasm base =
  let step d s =
    if Machine.is_ready s then s
    else Machine.view s base ~f:(fun s mem -> Dis.jump d mem s) in
  Machine.start ?is_code base roots ~f:(fun init mem ->
      Dis.run disasm mem ~stop_on ~return:ident ~init
        ~stopped:(fun d s -> step d (Machine.stopped s))
        ~hit:(fun d mem insn s ->
            step d @@
            let code = lift mem insn in
            if Insn.(may affect_control_flow) code ||
               has_jump (Insn.bil code)
            then
              Machine.jumped s mem (dests code) (delay code)
            else Machine.moved s mem)
        ~invalid:(fun d _ s -> step d (Machine.failed s s.addr)))

type t = {
  dis : (Dis.empty, Dis.empty) Dis.t;
  arch : arch;
  begs : Set.M(Addr).t;
  jmps : Machine.dsts Map.M(Addr).t;
  data : Set.M(Addr).t;
  mems : mem list;
}

let create ?(backend="llvm") arch =
  match Dis.create ~backend (Arch.to_string arch) with
  | Error err -> Error err
  | Ok dis -> Ok {
      dis;
      arch;
      begs = Set.empty (module Addr);
      jmps = Map.empty (module Addr);
      data = Set.empty (module Addr);
      mems = []
    }

let lifter arch mem insn =
  let open KB.Syntax in
  let code =
    KB.Object.create Theory.Program.cls >>= fun code ->
    KB.provide Arch.slot code (Some arch) >>= fun () ->
    KB.provide Memory.slot code (Some mem) >>= fun () ->
    KB.provide Insn.Slot.name code (Dis.Insn.name insn) >>= fun () ->
    KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
    code in
  match Bap_state.run Theory.Program.cls code with
  | Ok prog ->
    let bil = Insn.bil prog in
    let prog' = Insn.of_basic ~bil insn in
    KB.Value.merge ~on_conflict:`drop_right prog prog'
  | Error _ ->
    KB.Value.empty Theory.Program.cls


let scan ?(entries=[]) ?is_code self mem =
  let lifter = lifter self.arch in
  let roots =
    List.fold entries ~init:(Set.empty (module Addr)) ~f:Set.add in
  let {Machine.begs; jmps; data} =
    scan_mem ?is_code roots lifter self.dis mem in
  let jmps = Map.merge self.jmps jmps ~f:(fun ~key:_ -> function
      | `Left dsts | `Right dsts | `Both (_,dsts) -> Some dsts) in
  let begs = Set.union self.begs begs in
  let data = Set.union self.data data in
  {
    self with begs; data; jmps;
              mems = mem :: self.mems;
  }

let explore
    ?entry:start ?(follow=fun _ -> true) ~lift ~node ~edge ~init
    {begs; jmps; data; dis; mems} =
  let find_base addr =
    if Set.mem data addr then None
    else List.find mems ~f:(fun mem -> Memory.contains mem addr) in
  let blocks = Hashtbl.create (module Addr) in
  let edge_insert cfg src dst kind =
    Option.value_map dst ~default:cfg ~f:(fun dst ->
        edge src dst kind cfg) in
  let view ?len from mem = ok_exn (Memory.view ?words:len ~from mem) in
  let rec build cfg beg =
    if not (follow beg) then cfg,None
    else match Hashtbl.find blocks beg with
      | Some block -> cfg, Some block
      | None -> match find_base beg with
        | None -> cfg,None
        | Some mem ->
          let fin,len,insns =
            Dis.run dis (view beg mem)
              ~stop_on ~init:(beg,0,[]) ~return:ident
              ~hit:(fun dis mem insn (curr,len,insns) ->
                  let len = Memory.length mem + len in
                  let last = Memory.max_addr mem in
                  let next = Addr.succ last in
                  if Set.mem begs next || Map.mem jmps curr
                  then (curr,len,(mem,insn)::insns)
                  else Dis.jump dis (view next mem)
                      (next,len,(mem,insn)::insns)) in
          let mem = view ~len beg mem in
          let block = lift mem (List.rev insns) in
          let fall = Addr.succ (Memory.max_addr mem) in
          Hashtbl.add_exn blocks beg block;
          let cfg = node block cfg in
          match Map.find jmps fin with
          | None ->
            let cfg,dst = build cfg fall in
            edge_insert cfg block dst `Fall, Some block
          | Some dsts ->
            List.fold dsts ~init:cfg ~f:(fun cfg -> function
                | None,_ -> cfg
                | Some dst,kind ->
                  let dst = match kind with `Fall -> fall | _ -> dst in
                  let cfg,dst = build cfg dst in
                  edge_insert cfg block dst kind),Some block  in
  match start with
  | None -> Set.fold begs ~init ~f:(fun cfg beg ->
      fst (build cfg beg))
  | Some start -> fst (build init start)
