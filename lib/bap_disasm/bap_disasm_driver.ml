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


type dsts = {
  indirect : bool;
  resolved : Set.M(Addr).t;
}

module Machine : sig
  type task = private
    | Dest of {dst : addr; parent : task option}
    | Fall of {dst : addr; parent : task; delay : slot}
    | Jump of {src : addr; age: int; dsts : dsts; parent : task}
  and slot = private
    | Ready of task option
    | Delay

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

  type task =
    | Dest of {dst : addr; parent : task option}
    | Fall of {dst : addr; parent : task; delay : slot}
    | Jump of {src : addr; age: int; dsts : dsts; parent : task}
  and slot =
    | Ready of task option
    | Delay


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
    data = Set.add s.data addr;
    begs = Set.remove s.begs addr;
    usat = Set.remove s.usat addr;
    code = Set.remove s.code addr;
    jmps = Map.filter_map (Map.remove s.jmps addr) ~f:(fun dsts ->
        let resolved = Set.remove dsts.resolved addr in
        if Set.is_empty resolved && not dsts.indirect
        then None
        else Some {dsts with resolved});
  }

  let has_valid s dsts =
    dsts.indirect ||
    Set.exists dsts.resolved ~f:(fun dst ->
        not (Set.mem s.data dst))

  let pp_task ppf = function
    | Dest {dst; parent=None} ->
      Format.fprintf ppf "Root %a" Addr.pp dst
    | Dest {dst} ->
      Format.fprintf ppf "Dest %a" Addr.pp dst
    | Fall {dst} ->
      Format.fprintf ppf "Fall %a" Addr.pp dst
    | Jump {src; age} ->
      Format.fprintf ppf "Delay%d %a" age Addr.pp src

  let rec cancel task s = match task with
    | Dest {parent=None} -> s
    | Dest {parent=Some parent} | Fall {parent} | Jump {parent} ->
      match parent with
      | Dest {dst} | Fall {dst} -> cancel parent (mark_data s dst)
      | Jump {src; dsts} -> match task with
        | Fall {delay=(Ready (Some _) | Delay)} ->
          cancel parent (mark_data s src)
        | Fall _ when has_valid s dsts -> s
        | Fall _ | Dest _ -> cancel parent (mark_data s src)
        | Jump _ -> assert false

  let rec step s = match s.work with
    | [] ->
      if Set.is_empty s.usat then {s with stop = true}
      else step {s with work = [
          Dest {dst=Set.min_elt_exn s.usat; parent=None}
        ]}
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
      if Set.mem s.data src then step @@ cancel jump {s with work}
      else
        let resolved = Set.filter dsts.resolved ~f:(fun dst ->
            not (Set.mem s.data dst)) in
        let dsts = {dsts with resolved} in
        let init = {s with jmps = Map.add_exn s.jmps src dsts; work} in
        step @@
        Set.fold resolved ~init ~f:(fun s next ->
            if not (is_visited s next)
            then {s with work = Dest {dst=next; parent = Some jump} ::
                                s.work}
            else s)
    | Jump jmp as self :: Fall ({dst=next} as slot) :: work ->
      let delay = if jmp.age = 1 then Ready (Some self) else Delay in
      step {
        s with
        work = Fall {slot with delay} :: Jump {
            jmp with age = jmp.age-1; src = next;
          } :: work
      }
    | Jump jmp :: work -> step {
        s with work = Jump {jmp with age=0} :: work
      }

  let decoded s mem =
    let addr = Memory.min_addr mem in {
      s with code = Set.add s.code addr;
             usat = Set.remove s.usat addr
    }

  let jumped s mem dsts delay =
    let s = decoded s mem in
    let parent = s.curr in
    let src = Memory.min_addr mem in
    let jump = Jump {src; age=delay; dsts; parent} in
    let next = Addr.succ (Memory.max_addr mem) in
    let next = Fall {dst=next; parent=jump; delay = Ready None} in
    step {s with work = jump :: next :: s.work }

  let insert_delayed t = function
    | x :: xs -> x :: t :: xs
    | [] -> [t]

  let moved s mem =
    let parent = match s.curr with
      | Fall {delay=Ready (Some parent)} -> parent
      | _ -> s.curr in
    let next = Fall {
        dst = Addr.succ (Memory.max_addr mem);
        parent;
        delay = Ready None;
      } in
    let work = match s.curr with
      | Fall {delay = Delay} -> insert_delayed next s.work
      | _ -> next :: s.work in
    step @@ decoded {s with work} mem


  let failed s addr =
    step @@ cancel s.curr @@ mark_data s addr

  let stopped s =
    step @@ cancel s.curr @@ mark_data s s.addr

  let rec view s base ~empty ~ready =
    match Memory.view ~from:s.addr base with
    | Ok mem -> ready s mem
    | Error _ ->
      let s = match s.curr with
        | Fall _ as task ->
          cancel task s
        | _ -> s in
      match s.work with
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

let new_insn arch mem insn =
  KB.Object.create Theory.Program.cls >>= fun code ->
  KB.provide Arch.slot code (Some arch) >>= fun () ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>= fun () ->
  let insn = Insn.of_basic insn in
  KB.provide Theory.Program.Semantics.slot code insn >>| fun () ->
  code

let collect_dests arch mem insn =
  let width = Size.in_bits (Arch.addr_size arch) in
  let fall = Addr.to_bitvec (Addr.succ (Memory.max_addr mem)) in
  let init =
    {indirect = false; resolved = Set.empty (module Addr)} in
  new_insn arch mem insn >>= fun code ->
  KB.collect Theory.Program.Semantics.slot code >>= fun insn ->
  KB.Value.get Insn.Slot.dests insn |> function
  | None -> KB.return init
  | Some dests ->
    Set.to_sequence dests |>
    KB.Seq.fold ~init ~f:(fun {indirect;resolved} label ->
        KB.collect Theory.Label.addr label >>| function
        | Some d ->
          if Bitvec.(d <> fall)
          then {
            indirect;
            resolved = Set.add resolved (Word.create d width)
          } else {indirect; resolved}
        | None -> {indirect=true; resolved})


let delay arch mem insn =
  new_insn arch mem insn >>= fun code ->
  KB.collect Theory.Program.Semantics.slot code >>| fun insn ->
  KB.Value.get Insn.Slot.delay insn |> function
  | None -> 0
  | Some x -> x

let classify_mem mem =
  let empty = Set.empty (module Addr) in
  let base = Memory.min_addr mem in
  Seq.range 0 (Memory.length mem) |>
  KB.Seq.fold ~init:(empty,empty,empty) ~f:(fun (code,data,root) off ->
      let addr = Addr.(nsucc base off) in
      let slot = Some (Addr.to_bitvec addr) in
      KB.Object.scoped Theory.Program.cls @@ fun label ->
      KB.provide Theory.Label.addr label slot >>= fun () ->
      KB.collect Theory.Label.is_valid label >>= function
      | Some false -> KB.return (code,Set.add data addr,root)
      | r ->
        let code = if Option.is_none r then code
          else Set.add code addr in
        KB.collect Theory.Label.is_subroutine label >>| function
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
              collect_dests arch mem insn >>= fun dests ->
              if Set.is_empty dests.resolved &&
                 not dests.indirect then
                step d @@ Machine.moved s mem
              else
                delay arch mem insn >>= fun delay ->
                step d @@ Machine.jumped s mem dests delay)
          ~invalid:(fun d _ s -> step d (Machine.failed s s.addr)))
    ~empty:KB.return

type insns = Theory.Label.t list
type state = {
  begs : Set.M(Addr).t;
  jmps : dsts Map.M(Addr).t;
  data : Set.M(Addr).t;
  mems : mem list;
}

let init = {
  begs = Set.empty (module Addr);
  jmps = Map.empty (module Addr);
  data = Set.empty (module Addr);
  mems = []
}

let query_arch addr =
  KB.Object.scoped Theory.Program.cls @@ fun obj ->
  KB.provide Theory.Label.addr obj (Some addr) >>= fun () ->
  KB.collect Arch.slot obj

let scan mem s =
  let open KB.Syntax in
  let start = Word.to_bitvec (Memory.min_addr mem) in
  query_arch start >>= function
  | None -> KB.return s
  | Some arch -> match Dis.create (Arch.to_string arch) with
    | Error _ -> KB.return s
    | Ok dis ->
      scan_mem arch dis mem >>| fun {Machine.begs; jmps; data} ->
      let jmps = Map.merge s.jmps jmps ~f:(fun ~key:_ -> function
          | `Left dsts | `Right dsts | `Both (_,dsts) -> Some dsts) in
      let begs = Set.union s.begs begs in
      let data = Set.union s.data data in
      {begs; data; jmps; mems = mem :: s.mems}

let merge t1 t2 = {
  begs = Set.union t1.begs t2.begs;
  data = Set.union t1.data t2.data;
  mems = List.rev_append t2.mems t1.mems;
  jmps = Map.merge t1.jmps t2.jmps ~f:(fun ~key:_ -> function
      | `Left dsts | `Right dsts -> Some dsts
      | `Both (d1,d2) -> Some {
          indirect = d1.indirect || d2.indirect;
          resolved = Set.union d1.resolved d2.resolved;
        })
}

let list_insns ?(rev=false) insns =
  if rev then insns else List.rev insns

let rec insert pos x xs =
  if pos = 0 then x::xs else match xs with
    | x' :: xs -> x' :: insert (pos-1) x xs
    | [] -> [x]

let execution_order stack =
  KB.List.fold stack ~init:[] ~f:(fun insns insn ->
      KB.collect Theory.Program.Semantics.slot insn >>| fun s ->
      match KB.Value.get Insn.Slot.delay s with
      | None -> insn::insns
      | Some d -> insert d insn insns)

let always _ = KB.return true

let with_disasm beg cfg f =
  query_arch (Word.to_bitvec beg) >>= function
  | None -> KB.return (cfg,None)
  | Some arch ->
    match Dis.create (Arch.to_string arch) with
    | Error _ -> KB.return (cfg,None)
    | Ok dis -> f arch dis

let explore
    ?entry:start ?(follow=always) ~block ~node ~edge ~init
    {begs; jmps; data; mems} =
  let find_base addr =
    if Set.mem data addr then None
    else List.find mems ~f:(fun mem -> Memory.contains mem addr) in
  let blocks = Hashtbl.create (module Addr) in
  let edge_insert cfg src dst = match dst with
    | None -> KB.return cfg
    | Some dst -> edge src dst cfg in
  let view ?len from mem = ok_exn (Memory.view ?words:len ~from mem) in
  let rec build cfg beg =
    if Set.mem data beg then KB.return (cfg,None)
    else follow beg >>= function
      | false -> KB.return (cfg,None)
      | true -> with_disasm beg cfg @@ fun arch dis ->
        match Hashtbl.find blocks beg with
        | Some block -> KB.return (cfg, Some block)
        | None -> match find_base beg with
          | None -> KB.return (cfg,None)
          | Some base ->
            Dis.run dis (view beg base) ~stop_on:[`Valid]
              ~init:(beg,0,[]) ~return:KB.return
              ~hit:(fun s mem insn (curr,len,insns) ->
                  new_insn arch mem insn >>= fun insn ->
                  let len = Memory.length mem + len in
                  let last = Memory.max_addr mem in
                  let next = Addr.succ last in
                  if Set.mem data next && not (Map.mem jmps curr)
                  then assert false
                  else
                  if Set.mem begs next || Map.mem jmps curr
                  then KB.return (curr, len, insn::insns)
                  else Dis.jump s (view next base)
                      (next, len, insn::insns))
            >>= fun (fin,len,insns) ->
            let mem = view ~len beg base in
            block mem insns >>= fun block ->
            let fall = Addr.succ (Memory.max_addr mem) in
            Hashtbl.add_exn blocks beg block;
            node block cfg >>= fun cfg ->
            match Map.find jmps fin with
            | None ->
              build cfg fall >>= fun (cfg,dst) ->
              edge_insert cfg block dst >>= fun cfg ->
              KB.return (cfg, Some block)
            | Some {resolved=dsts} ->
              Set.to_sequence (Set.add dsts fall) |>
              KB.Seq.fold ~init:cfg ~f:(fun cfg dst ->
                  build cfg dst >>= fun (cfg,dst) ->
                  edge_insert cfg block dst) >>= fun cfg ->
              KB.return (cfg,Some block)  in
  match start with
  | None ->
    Set.to_sequence begs |>
    KB.Seq.fold ~init ~f:(fun cfg beg ->
        build cfg beg >>| fst)
  | Some start -> build init start >>| fst
