open Core_kernel
open Bap_types.Std
open Bap_core_theory
open Bap_image_std

open KB.Syntax

module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn

type full_insn = Dis.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type edge = [`Jump | `Cond | `Fall] [@@deriving compare]


module Machine : sig
  type dsts = addr list
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
    mem ->
    code:Set.M(Addr).t ->
    data:Set.M(Addr).t ->
    init:Set.M(Addr).t ->
    empty:(state -> 'a) ->
    ready:(state -> mem -> 'a) -> 'a

  val view : state -> mem ->
    empty:(state -> 'a) ->
    ready:(state -> mem -> 'a) -> 'a

  val failed : state -> addr -> state
  val jumped : state -> mem -> dsts -> int -> state
  val stopped : state -> state
  val moved  : state -> mem -> state
  val is_ready : state -> bool
end = struct
  type dsts = addr list

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
    List.count dsts ~f:(fun dst -> not (Set.mem s.data dst))

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
      List.fold dsts ~init ~f:(fun s next ->
          if not (is_visited s next)
          then {s with work = Dest {dst=next; parent = Some jump} ::
                              s.work}
          else s)
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

  let rec view s base ~empty ~ready =
    match Memory.view ~from:s.addr base with
    | Ok mem -> ready s mem
    | Error _ -> match s.work with
      | [] -> empty (step s)
      | _  -> view (step s) base ~empty ~ready

  let start mem ~code ~data ~init =
    let work = init_work init in
    let start = match Set.min_elt init with
      | Some start -> start
      | None -> Memory.min_addr mem in
    view {
      work; data; usat=code;
      addr = start;
      curr = Dest {dst = start; parent = None};
      stop = false;
      begs = Set.empty (module Addr);
      jmps = Map.empty (module Addr);
      code = Set.empty (module Addr);
    } mem
end



let dests : (Theory.program, Set.M(Theory.Label).t option) KB.slot =
  let data = KB.Domain.optional ~equal:Set.equal "label" in
  KB.Class.property ~package:"bap.std" Theory.Program.cls
    "dests" data

let new_insn arch mem insn =
  KB.Object.create Theory.Program.cls >>= fun code ->
  KB.provide Arch.slot code (Some arch) >>= fun () ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Insn.Slot.name code (Dis.Insn.name insn) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let collect_dests arch mem insn =
  let width = Size.in_bits (Arch.addr_size arch) in
  let fall = Addr.to_bitvec (Addr.succ (Memory.max_addr mem)) in
  new_insn arch mem insn >>= fun code ->
  KB.collect dests code >>= function
  | None -> KB.return []
  | Some dests ->
    Set.to_sequence dests |>
    KB.Seq.filter_map ~f:(fun label ->
        KB.collect Theory.Label.addr label >>| function
        | Some d when Bitvec.(d <> fall) -> Some (Word.create d width)
        | _ -> None) >>|
    Seq.to_list_rev

let delay arch mem insn =
  new_insn arch mem insn >>= fun code ->
  KB.collect Insn.Slot.delay code >>| function
  | None -> 0
  | Some x -> x


let attr name =
  let bool_t = KB.Domain.optional ~equal:Bool.equal "bool" in
  KB.Class.property ~package:"bap.std" Theory.Program.cls name bool_t


let is_valid = attr "is-valid"
let is_subroutine = attr "is-subroutine"

let classify_mem mem =
  let empty = Set.empty (module Addr) in
  let base = Memory.min_addr mem in
  Seq.range 0 (Memory.length mem) |>
  KB.Seq.fold ~init:(empty,empty,empty) ~f:(fun (code,data,root) off ->
      let addr = Addr.(nsucc base off) in
      Theory.Label.for_addr (Addr.to_bitvec addr) >>= fun label ->
      KB.collect is_valid label >>= function
      | Some false -> KB.return (code,Set.add data addr,root)
      | r ->
        let code = if Option.is_none r then code
          else Set.add code addr in
        KB.collect is_subroutine label >>| function
        | Some true -> (code,data,Set.add root addr)
        | _ -> (code,data,root))

let scan_mem arch disasm base : Machine.state KB.t =
  classify_mem base >>= fun (code,data,init) ->
  let step d s =
    if Machine.is_ready s then KB.return s
    else Machine.view s base ~ready:(fun s mem -> Dis.jump d mem s)
        ~empty:KB.return in
  Machine.start base ~code ~data ~init
    ~ready:(fun init mem ->
        Dis.run disasm mem ~stop_on:[`Valid]
          ~return:KB.return ~init
          ~stopped:(fun d s -> step d (Machine.stopped s))
          ~hit:(fun d mem insn s ->
              collect_dests arch mem insn >>= function
              | [] -> step d @@ Machine.moved s mem
              | dests ->
                delay arch mem insn >>= fun delay ->
                step d @@ Machine.jumped s mem dests delay)
          ~invalid:(fun d _ s -> step d (Machine.failed s s.addr)))
    ~empty:KB.return

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

let scan self mem =
  let open KB.Syntax in
  scan_mem self.arch self.dis mem >>|
  fun {Machine.begs; jmps; data} ->
  let jmps = Map.merge self.jmps jmps ~f:(fun ~key:_ -> function
      | `Left dsts | `Right dsts | `Both (_,dsts) -> Some dsts) in
  let begs = Set.union self.begs begs in
  let data = Set.union self.data data in
  {self with begs; data; jmps; mems = mem :: self.mems}


type insns = (mem * Theory.Label.t) list

let list_insns ?(rev=false) insns =
  if rev then List.rev insns else insns

let rec insert pos x xs =
  if pos = 0 then x::xs else match xs with
    | x' :: xs -> x' :: insert (pos-1) x xs
    | [] -> [x]

let execution_order ~delay stack =
  KB.List.fold stack ~init:[] ~f:(fun insns (mem,insn) ->
      delay insn >>| fun d -> insert d (mem,insn) insns)

let always _ = KB.return true

let explore
    ?entry:start ?(follow=always) ~block ~node ~edge ~init
    {begs; jmps; data; dis; mems; arch} =
  let find_base addr =
    if Set.mem data addr then None
    else List.find mems ~f:(fun mem -> Memory.contains mem addr) in
  let blocks = Hashtbl.create (module Addr) in
  let edge_insert cfg src dst = match dst with
    | None -> KB.return cfg
    | Some dst -> edge src dst cfg in
  let view ?len from mem = ok_exn (Memory.view ?words:len ~from mem) in
  let rec build cfg beg =
    follow beg >>= function
    | false -> KB.return (cfg,None)
    | true -> match Hashtbl.find blocks beg with
      | Some block -> KB.return (cfg, Some block)
      | None -> match find_base beg with
        | None -> KB.return (cfg,None)
        | Some mem ->
          Dis.run dis (view beg mem) ~stop_on:[`Valid]
            ~init:(beg,0,[]) ~return:KB.return
            ~hit:(fun s mem insn (curr,len,insns) ->
                new_insn arch mem insn >>= fun insn ->
                let len = Memory.length mem + len in
                let last = Memory.max_addr mem in
                let next = Addr.succ last in
                if Set.mem begs next || Map.mem jmps curr
                then KB.return (curr,len,(mem,insn)::insns)
                else Dis.jump s (view next mem)
                    (next,len,(mem,insn)::insns))
          >>= fun (fin,len,insns) ->
          let mem = view ~len beg mem in
          block mem insns >>= fun block ->
          let fall = Addr.succ (Memory.max_addr mem) in
          Hashtbl.add_exn blocks beg block;
          node block cfg >>= fun cfg ->
          match Map.find jmps fin with
          | None ->
            build cfg fall >>= fun (cfg,dst) ->
            edge_insert cfg block dst >>= fun cfg ->
            KB.return (cfg, Some block)
          | Some dsts ->
            KB.List.fold (fall::dsts) ~init:cfg ~f:(fun cfg dst ->
                build cfg dst >>= fun (cfg,dst) ->
                edge_insert cfg block dst) >>= fun cfg ->
            KB.return (cfg,Some block)  in
  match start with
  | None ->
    Set.to_sequence begs |>
    KB.Seq.fold ~init ~f:(fun cfg beg ->
        build cfg beg >>| fst)
  | Some start -> build init start >>| fst
