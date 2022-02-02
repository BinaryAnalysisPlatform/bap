open Core_kernel
open Bap_types.Std
open Bap_core_theory
open Bap_image_std

include Bap_main.Loggers()

open KB.Syntax

module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn
module Target = Theory.Target

type full_insn = Dis.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type edge = [`Jump | `Cond | `Fall] [@@deriving compare]

type encoding = {
  coding : Theory.Language.t;
  target : Theory.Target.t;
} [@@deriving bin_io, compare, equal]

let unknown = {
  coding = Theory.Language.unknown;
  target = Theory.Target.unknown;
}

type jump = {
  encoding : encoding;
  call : bool;
  barrier : bool;
  indirect : bool;
  resolved : Addr.Set.t;
} [@@deriving bin_io, equal]

let pp_encoding ppf {coding} =
  Format.fprintf ppf "%a" Theory.Language.pp coding

let order_encoding = KB.Domain.order Theory.Language.domain

module Machine : sig

  (*
     - Dest {dst; parent} - [dst] is either a destination of
       a [Jump t], when parent is [Some (Jump t)], or an initial
       root (function start), when parent is [None].

     - Fall {dst; parent} - is a fallthrough task either from
       [parent];

     - Jump {src; dsts} - is a jump from src to the set of dests,
       potentially delayed and waiting until its age is 0.
  *)
  type task = private
    | Dest of {dst : addr; parent : task option; encoding : encoding}
    | Fall of {dst : addr; parent : task; delay : slot; encoding : encoding}
    | Jump of {src : addr; age: int; dsts : jump; parent : task; encoding : encoding }
  and slot = private
    | Ready of task option
    | Delay
  [@@deriving bin_io]

  type state = private {
    stop : bool;                          (* true if finished *)
    work : task list;                     (* work list*)
    debt : task list;                     (* work that we can't finish *)
    curr : task;                          (* current task *)
    addr : addr;                          (* current address *)
    dels : Set.M(Addr).t;                 (* delay slots *)
    begs : task list Map.M(Addr).t;       (* beginings of basic blocks *)
    jmps : jump Map.M(Addr).t;            (* jumps  *)
    code : encoding Map.M(Addr).t;        (* all valid instructions *)
    data : Set.M(Addr).t;                 (* all non-instructions *)
    usat : Set.M(Addr).t;                 (* unsatisfied constraints *)
  }

  val start :
    mem ->
    debt:task list ->
    code:Set.M(Addr).t ->
    data:Set.M(Addr).t ->
    init:Set.M(Addr).t ->
    empty:(state -> 'a) ->
    ready:(state -> encoding -> mem -> 'a) -> 'a

  val view : state -> mem ->
    empty:(state -> 'a) ->
    ready:(state -> encoding -> mem -> 'a) -> 'a

  val is_ready : state -> bool
  val encoding : state -> encoding
  val switch : state -> encoding -> state
  val moved  : state -> encoding -> mem -> state
  val jumped : state -> encoding -> mem -> jump -> int -> state
  val failed : state -> encoding -> addr -> state
  val stopped : state -> encoding -> state
  val skipped : state -> addr -> state
end = struct

  type task =
    | Dest of {dst : addr; parent : task option; encoding : encoding}
    | Fall of {dst : addr; parent : task; delay : slot; encoding : encoding}
    | Jump of {src : addr; age: int; dsts : jump; parent : task; encoding : encoding}
  and slot =
    | Ready of task option
    | Delay
  [@@deriving bin_io]


  let init_work init roots =
    Set.to_sequence ~order:`Decreasing roots |>
    Seq.fold ~init ~f:(fun work root -> Dest {
        dst=root;
        parent=None;
        encoding=unknown
      } :: work)

  type state = {
    stop : bool;
    work : task list;
    debt : task list;
    curr : task;
    addr : addr;
    dels : Set.M(Addr).t;
    begs : task list Map.M(Addr).t;
    jmps : jump Map.M(Addr).t;
    code : encoding Map.M(Addr).t;
    data : Set.M(Addr).t;
    usat : Set.M(Addr).t;
  }

  let is_code s addr = Map.mem s.code addr
  let is_data s addr = Set.mem s.data addr
  let is_visited s addr = is_code s addr || is_data s addr
  let is_ready s = s.stop
  let wrong_encoding s addr {coding} =
    match Map.find s.code addr with
    | None -> false
    | Some {coding=other} ->
      Result.is_error @@
      KB.Domain.join Theory.Language.domain
        other coding

  let has_valid s dsts =
    dsts.indirect ||
    Set.exists dsts.resolved ~f:(fun dst ->
        not (Set.mem s.data dst))

  let pp_task ppf = function
    | Dest {dst; parent=None} ->
      Format.fprintf ppf "root-%a" Addr.pp dst
    | Dest {dst} ->
      Format.fprintf ppf "dest-%a" Addr.pp dst
    | Fall {dst} ->
      Format.fprintf ppf "fall-%a" Addr.pp dst
    | Jump {src; age=0} ->
      Format.fprintf ppf "jump-%a" Addr.pp src
    | Jump {src; age} ->
      Format.fprintf ppf "del%d-%a" age Addr.pp src

  let sexp_of_task t = Sexp.Atom (Format.asprintf "%a" pp_task t)


  (** [revert t s] reverts the chain that produced the task [t].*)
  let rec revert task s = match task with
    | Dest {parent=None} -> s
    | Dest {parent=Some parent} | Fall {parent} | Jump {parent} ->
      match parent with
      | Fall _ | Dest _ -> cancel parent s
      | Jump _ -> match task with
        | Fall {delay=(Ready (Some _) | Delay)} ->
          cancel parent s
        | _ -> s

  (** [cancel t s] marks the address of [t] as data and reverts [t]  *)
  and cancel task s = revert task @@ match task with
    | Dest {dst} | Fall {dst} -> delete s dst
    | Jump {dsts} when has_valid s dsts -> s
    | Jump {src} -> delete s src

  and revert_incoming s addr = match Map.find s.begs addr with
    | None -> s
    | Some tasks ->
      List.fold_right tasks ~f:revert ~init:{
        s with begs = Map.remove s.begs addr;
      }

  and delete s addr = {
    (revert_incoming s addr) with
    data = Set.add s.data addr;
    begs = Map.remove s.begs addr;
    usat = Set.remove s.usat addr;
    code = Map.remove s.code addr;
    jmps = Map.remove s.jmps addr;
  }

  let is_slot s addr = Set.mem s.dels addr

  let task_encoding = function
    | Fall {encoding} | Dest {encoding} | Jump {encoding} -> encoding

  let rec step s = match s.work with
    | [] when Set.is_empty s.usat -> {s with stop = true}
    | [] -> restart s
    | Dest {dst=next; encoding} as curr :: work
      when wrong_encoding s next encoding ->
      step @@ revert curr {s with work}
    | Dest {dst=next} as curr :: work
      when is_data s next || is_slot s next ->
      step @@ revert curr {s with work}
    | Dest {dst=next} as curr :: work ->
      let s = {s with begs = Map.add_multi s.begs next curr} in
      if is_visited s next
      then step {s with work}
      else {s with work; addr=next; curr}
    | Fall {dst=next; encoding} as curr :: work ->
      if is_code s next
      then
        if is_slot s next
        then step @@ revert curr {s with work}
        else if wrong_encoding s next encoding
        then step @@ revert curr {s with work}
        else step {s with begs = Map.add_multi s.begs next curr; work}
      else if is_data s next then step @@ revert curr {s with work}
      else {s with work; addr=next; curr}
    | (Jump {src; dsts} as jump) :: ([] as work)
    | (Jump {src; dsts; age=0} as jump) :: work ->
      if Set.mem s.data src
      then step @@ revert jump {s with work}
      else
        let resolved = Set.diff dsts.resolved s.data in
        if Set.is_empty resolved && not dsts.indirect
        then step@@revert jump {s with work}
        else
          let dsts = {dsts with resolved} in
          let init = {s with jmps = Map.add_exn s.jmps src dsts; work} in
          step @@
          Set.fold resolved ~init ~f:(fun s next -> {
                s with
                work = Dest {
                    dst=next; parent = Some jump; encoding = dsts.encoding;
                  } :: s.work})
    | Jump jmp as self :: Fall ({dst=next} as slot) :: work ->
      let s = revert_incoming s next in
      let delay = if jmp.age = 1 then Ready (Some self) else Delay in
      let jump = Jump {
          jmp with age = jmp.age-1; src = next;
        } in
      step {
        s with
        work = Fall {slot with delay} :: jump :: work;
        dels = Set.add s.dels next;
      }
    | Jump jmp :: work -> step {
        s with work = Jump {jmp with age=0} :: work
      }
  and restart s = step {
      s with work = [
      Dest {
        dst=Set.min_elt_exn s.usat;
        parent=None;
        encoding=unknown
      }
    ]}

  let decoded s mem encoding =
    let addr = Memory.min_addr mem in {
      s with code = Map.add_exn s.code addr encoding;
             usat = Set.remove s.usat addr
    }

  let jumped s encoding mem dsts delay =
    let s = decoded s mem encoding in
    let parent = s.curr in
    let src = Memory.min_addr mem in
    let jump = Jump {src; age=delay; dsts; parent; encoding} in
    let dst = Addr.succ (Memory.max_addr mem) in
    let next =
      if dsts.barrier && delay = 0
      then Dest {dst; parent=None; encoding=unknown}
      else Fall {dst; parent=jump; delay = Ready None; encoding} in
    step {s with work = jump :: next :: s.work }

  let insert_delayed t = function
    | x :: xs -> x :: t :: xs
    | [] -> [t]

  let moved s encoding mem =
    let parent = match s.curr with
      | Fall {delay=Ready (Some parent)} -> parent
      | _ -> s.curr in
    let next = Addr.succ (Memory.max_addr mem) in
    let next = match parent with
      | Jump {dsts={barrier=true}} -> Dest {
          dst=next;
          parent=None;
          encoding=unknown
        }
      | parent -> Fall {
          dst = next;
          parent;
          delay = Ready None;
          encoding;
        } in
    let work = match s.curr with
      | Fall {delay = Delay} -> insert_delayed next s.work
      | _ -> next :: s.work in
    step @@ decoded {s with work} mem encoding

  let failed s _ _ = step @@ cancel s.curr s

  let skipped s addr =
    step {s with usat = Set.remove s.usat addr }


  let stopped s _ =
    step @@ cancel s.curr s

  let with_encoding encoding = function
    | Fall s -> Fall {s with encoding}
    | Dest s -> Dest {s with encoding}
    | Jump _ as j -> j

  let switch s encodings = {
    s with curr = with_encoding encodings s.curr
  }

  let encoding s = task_encoding s.curr


  let rec view s base ~empty ~ready =
    if s.stop then empty s
    else match Memory.view ~from:s.addr base with
      | Ok mem -> ready s (task_encoding s.curr) mem
      | Error _ ->
        let s = match s.curr with
          | Fall _ | Jump _ as task -> revert task s
          | t -> {s with debt = t :: s.debt} in
        match s.work with
        | [] -> empty (step s)
        | _ -> view (step s) base ~empty ~ready

  let start mem ~debt ~code ~data ~init =
    let init = if Set.is_empty init
      then Set.singleton (module Addr) (Memory.min_addr mem)
      else init in
    let work = init_work debt init in
    let start = Set.min_elt_exn init in
    view {
      work; data; usat=code;
      addr = start;
      debt = [];
      curr = Dest {dst = start; parent = None; encoding=unknown};
      stop = false;
      dels = Set.empty (module Addr);
      begs = Map.empty (module Addr);
      jmps = Map.empty (module Addr);
      code = Map.empty (module Addr);
    } mem
end

let label_for_mem mem =
  let addr = Addr.to_bitvec (Memory.min_addr mem) in
  Theory.Label.for_addr addr

let new_insn mem insn =
  label_for_mem mem >>= fun code ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let get_encoding label =
  Theory.Label.target label >>= fun target ->
  KB.collect Theory.Label.encoding label >>| fun coding ->
  {coding; target}

let merge_encodings x y =
  match KB.Domain.join Theory.Language.domain x.coding y.coding with
  | Ok coding -> KB.return {x with coding}
  | Error mismatch -> KB.fail mismatch


let collect_dests source code =
  KB.collect Theory.Semantics.slot code >>= fun insn ->
  let init = {
    encoding=source;
    call = Insn.(is call insn);
    barrier = Insn.(is barrier insn);
    indirect = false;
    resolved = Set.empty (module Addr)
  } in
  KB.Value.get Insn.Slot.dests insn |> function
  | None -> KB.return init
  | Some dests ->
    Set.to_sequence dests |>
    KB.Seq.fold ~init ~f:(fun dest label ->
        get_encoding label >>=
        merge_encodings dest.encoding >>= fun encoding ->
        KB.collect Theory.Label.addr label >>| function
        | Some d -> {
            dest with
            encoding;
            resolved =
              Set.add dest.resolved @@
              Word.code_addr encoding.target d
          }
        | None -> {dest with indirect=true; encoding})

let pp_addr_opt ppf = function
  | None -> Format.fprintf ppf "Unk"
  | Some addr -> Format.fprintf ppf "%a" Bitvec.pp addr

let delay code =
  KB.collect Theory.Semantics.slot code >>| fun insn ->
  KB.Value.get Insn.Slot.delay insn |> function
  | None -> 0
  | Some x -> x

let unit_for_mem mem =
  let addr = Addr.to_bitvec @@ Memory.min_addr mem in
  KB.Object.scoped Theory.Program.cls @@ fun label ->
  KB.provide Theory.Label.addr label (Some addr) >>= fun () ->
  KB.collect Theory.Label.unit label

let classify mem =
  let empty = Set.empty (module Addr) in
  let base = Memory.min_addr mem in
  unit_for_mem mem >>= fun unit ->
  Seq.range 0 (Memory.length mem) |>
  KB.Seq.fold ~init:(empty,empty,empty) ~f:(fun (code,data,root) off ->
      let addr = Addr.(nsucc base off) in
      let slot = Some (Addr.to_bitvec addr) in
      KB.Object.scoped Theory.Program.cls @@ fun label ->
      KB.provide Theory.Label.addr label slot >>= fun () ->
      KB.provide Theory.Label.unit label unit >>= fun () ->
      KB.collect Theory.Label.is_valid label >>= function
      | Some false -> KB.return (code,Set.add data addr,root)
      | r ->
        let code = if Option.is_none r then code
          else Set.add code addr in
        KB.collect Theory.Label.is_subroutine label >>| function
        | Some true -> (code,data,Set.add root addr)
        | _ -> (code,data,root))

let create_disassembler {target; coding} =
  Dis.lookup target coding

let switch encoding s =
  match create_disassembler encoding with
  | Error _ -> s
  | Ok dis -> Dis.switch s dis

let is_known {coding} = not (Theory.Language.is_unknown coding)

let disassemble ~code ~data ~funs debt base : Machine.state KB.t =
  unit_for_mem base >>= fun unit ->
  let rec next_encoding state current mem f =
    let addr = Memory.min_addr mem in
    KB.Object.scoped Theory.Program.cls @@ fun obj ->
    KB.provide Theory.Label.addr obj (Some (Word.to_bitvec addr)) >>= fun () ->
    KB.provide Theory.Label.unit obj unit >>= fun () ->
    get_encoding obj >>= fun encoding ->
    match is_known encoding, is_known current with
    | false,false -> skip state addr f
    | true,false -> f state encoding mem
    | false,true -> f state current mem
    | true,true ->
      if equal_encoding current encoding
      then f state current mem
      else fail state encoding addr f
  and fail state encoding addr f =
    warning "ambiguous encoding for %a, rejecting" Addr.pp addr;
    Machine.view (Machine.failed state encoding addr) base
      ~empty:KB.return
      ~ready:(fun state current mem ->
          next_encoding state current mem f)
  and skip state addr f =
    warning "encoding for %a is unknown, skipping" Addr.pp addr;
    Machine.view (Machine.skipped state addr) base
      ~empty:KB.return
      ~ready:(fun state current mem ->
          next_encoding state current mem f) in
  let step d s =
    if Machine.is_ready s then KB.return s
    else Machine.view s base
        ~ready:(fun s encoding mem ->
            next_encoding s encoding mem @@
            fun s encoding mem ->
            Dis.jump (switch encoding d) mem @@
            Machine.switch s encoding)
        ~empty:KB.return in
  Machine.start base ~debt ~code ~data ~init:funs
    ~ready:(fun init encoding mem ->
        next_encoding init encoding mem @@
        fun init encoding mem ->
        match create_disassembler encoding with
        | Error _ ->
          warning "unable to disassemble instruction at %a \
                   with encoding %a"
            Addr.pp (Memory.min_addr mem) pp_encoding encoding;
          KB.return init
        | Ok disasm ->
          Dis.run disasm mem ~stop_on:[`Valid]
            ~return:KB.return ~init:(Machine.switch init encoding)
            ~stopped:(fun d s -> step d (Machine.stopped s encoding))
            ~hit:(fun d mem insn s ->
                KB.catch begin
                  new_insn mem insn >>= fun label ->
                  let encoding = Machine.encoding s in
                  KB.provide Theory.Label.encoding label encoding.coding >>= fun () ->
                  collect_dests encoding label >>= fun dests ->
                  if Set.is_empty dests.resolved && not dests.indirect
                  then step d @@ Machine.moved s encoding mem
                  else
                    delay label >>= fun delay ->
                    step d @@ Machine.jumped s encoding mem dests delay
                end @@ fun problem ->
                warning "rejecting %a due to a conflict %a"
                  Memory.pp mem KB.Conflict.pp problem;
                step d (Machine.failed s encoding s.addr))
            ~invalid:(fun d mem s ->
                info "rejecting %a as an invalid instruction"
                  Memory.pp mem;
                step d (Machine.failed s encoding s.addr)))
    ~empty:KB.return

type insns = Theory.Label.t list

type state = {
  funs : Addr.Set.t;
  begs : Addr.Set.t;
  jmps : jump Addr.Map.t;
  data : Addr.Set.t;
  mems : mem list;
  debt : Machine.task list;
} [@@deriving bin_io]

let init = {
  funs = Set.empty (module Addr);
  begs = Set.empty (module Addr);
  jmps = Map.empty (module Addr);
  data = Set.empty (module Addr);
  mems = [];
  debt = [];
}

let forget_debt s = {s with debt=[]}

let equal x y =
  phys_equal x y ||
  Set.equal x.begs y.begs &&
  Map.equal equal_jump x.jmps y.jmps

let subroutines x = x.funs
let blocks x = x.begs
let jump {jmps} = Map.find jmps

let is_data {data} = Set.mem data
let is_subroutine {funs} = Set.mem funs
let is_jump {jmps} = Map.mem jmps
let is_block {begs} = Set.mem begs

let destinations dsts = dsts.resolved
let is_call dsts = dsts.call
let is_barrier dsts = dsts.barrier


let commit_calls jmps =
  Map.to_sequence jmps |>
  KB.Seq.fold ~init:Addr.Set.empty ~f:(fun calls (_,dsts) ->
      if dsts.call then
        Set.to_sequence dsts.resolved |>
        KB.Seq.fold ~init:calls ~f:(fun calls addr ->
            Theory.Label.for_addr (Word.to_bitvec addr) >>= fun dst ->
            KB.provide Theory.Label.is_valid dst (Some true) >>= fun () ->
            KB.provide Theory.Label.is_subroutine dst (Some true) >>| fun () ->
            Set.add calls addr)
      else KB.return calls)


let owns mem s =
  List.exists s.debt ~f:(function
      | Machine.Dest {dst} -> Memory.contains mem dst
      | _ -> false)

let empty = Addr.Set.empty

let merge_dests d1 d2 = {
  encoding = d1.encoding;
  call = d1.call || d2.call;
  barrier = d1.barrier || d2.barrier;
  indirect = d1.indirect || d2.indirect;
  resolved = Set.union d1.resolved d2.resolved;
}

let scan_step ?(code=empty) ?(data=empty) ?(funs=empty) s mem =
  disassemble ~code ~data ~funs s.debt mem >>=
  fun {Machine.begs; jmps; data; debt; dels} ->
  let jmps = Map.merge s.jmps jmps ~f:(fun ~key:_ -> function
      | `Left dsts | `Right dsts -> Some dsts
      | `Both (d1,d2) -> Some (merge_dests d1 d2)) in
  let (-) = Set.diff in
  let data = Set.union s.data data in
  let begs = Set.of_map_keys begs - data in
  let funs = Set.union s.funs funs - data in
  let begs = Set.union s.begs begs - dels in
  let jmps = Map.filter_map jmps ~f:(fun dsts ->
      let resolved = dsts.resolved - data in
      if Set.is_empty resolved && not dsts.indirect
      then None
      else Some {dsts with resolved}) in
  let s = {funs; begs; data; jmps; mems = s.mems; debt} in
  commit_calls s.jmps >>| fun funs ->
  {s with funs = Set.union s.funs funs - dels}

let already_scanned mem s =
  let start = Memory.min_addr mem in
  List.exists s.mems ~f:(fun mem ->
      Memory.contains mem start)

let scan mem s =
  let open KB.Syntax in
  if already_scanned mem s
  then KB.return s
  else
    classify mem >>= fun (code,data,funs) ->
    scan_step ~code ~data ~funs s mem >>= fun s ->
    KB.List.fold s.mems
      ~f:(fun s mem ->
          if owns mem s then scan_step s mem
          else !!s)
      ~init:{s with mems = mem :: s.mems}


let merge t1 t2 = {
  funs = Set.union t1.funs t2.funs;
  begs = Set.union t1.begs t2.begs;
  data = Set.union t1.data t2.data;
  mems = List.rev_append t2.mems t1.mems;
  debt = List.rev_append t2.debt t1.debt;
  jmps = Map.merge t1.jmps t2.jmps ~f:(fun ~key:_ -> function
      | `Left dsts | `Right dsts -> Some dsts
      | `Both (d1,d2) -> Some (merge_dests d1 d2))
}

let list_insns ?(rev=false) insns =
  if rev then insns else List.rev insns

let rec insert pos x xs =
  if pos = 0 then x::xs else match xs with
    | x' :: xs -> x' :: insert (pos-1) x xs
    | [] -> [x]

let execution_order stack =
  KB.List.fold stack ~init:[] ~f:(fun insns insn ->
      KB.collect Theory.Semantics.slot insn >>| fun s ->
      match KB.Value.get Insn.Slot.delay s with
      | None -> insn::insns
      | Some d -> insert d insn insns)

let always _ = KB.return true

let with_disasm beg blocks cfg f =
  Theory.Label.for_addr (Word.to_bitvec beg) >>=
  get_encoding >>= fun encoding ->
  match create_disassembler encoding with
  | Error _ ->
    warning "unable to explore block %a with encoding %a"
      Word.pp beg pp_encoding encoding;
    KB.return (blocks,cfg,None)
  | Ok dis -> f encoding dis

let explore
    ?entries ?entry:start ?(follow=always) ~block ~node ~edge ~init
    ({begs; jmps; data; mems} as state) =
  let find_base addr =
    if Set.mem data addr then None
    else List.find mems ~f:(fun mem -> Memory.contains mem addr) in
  let edge_insert cfg src dst = match dst with
    | None -> KB.return cfg
    | Some dst -> edge src dst cfg in
  let view ?len from mem = Memory.view_exn ?words:len ~from mem in
  let rec build blocks cfg beg =
    if Set.mem data beg
    then KB.return (blocks,cfg,None)
    else follow beg >>= function
      | false -> KB.return (blocks,cfg,None)
      | true -> with_disasm beg blocks cfg @@ fun _encoding dis ->
        match Map.find blocks beg with
        | Some block -> KB.return (blocks,cfg,Some block)
        | None -> match find_base beg with
          | None -> KB.return (blocks,cfg,None)
          | Some base ->
            Dis.run dis (view beg base) ~stop_on:[`Valid]
              ~init:(beg,0,[]) ~return:KB.return
              ~hit:(fun s mem insn (curr,len,insns) ->
                  new_insn mem insn >>= fun insn ->
                  let len = Memory.length mem + len in
                  let last = Memory.max_addr mem in
                  let next = Addr.succ last in
                  let is_terminator =
                    Set.mem begs next ||
                    Map.mem jmps curr ||
                    Set.mem data next in
                  if is_terminator
                  then KB.return (curr, len, insn::insns)
                  else Dis.jump s (view next base)
                      (next, len, insn::insns))
            >>= fun (fin,len,insns) ->
            let mem = view ~len beg base in
            block mem insns >>= fun block ->
            let fall = Addr.succ (Memory.max_addr mem) in
            let blocks = Map.add_exn blocks beg block in
            node block cfg >>= fun cfg ->
            match Map.find jmps fin with
            | None ->
              build blocks cfg fall >>= fun (blocks,cfg,dst) ->
              edge_insert cfg block dst >>| fun cfg ->
              blocks,cfg,Some block
            | Some {resolved=dsts; barrier} ->
              let dsts = if barrier then dsts
                else Set.add dsts fall in
              Set.to_sequence dsts |>
              KB.Seq.fold ~init:(blocks,cfg) ~f:(fun (blocks,cfg) dst ->
                  build blocks cfg dst >>= fun (blocks,cfg,dst) ->
                  edge_insert cfg block dst >>| fun cfg ->
                  blocks,cfg) >>| fun (blocks,cfg) ->
              blocks,cfg,Some block in
  let entries = match start,entries with
    | None,None -> Set.to_sequence state.begs
    | Some beg,None -> Seq.singleton beg
    | None, Some begs -> begs
    | Some beg, Some begs -> Seq.cons beg begs in
  let empty = Map.empty (module Addr) in
  KB.Seq.fold entries ~init:(empty,init) ~f:(fun (blocks,cfg) beg ->
      build blocks cfg beg >>| fun (blocks,cfg,_) ->
      blocks,cfg) >>| snd
