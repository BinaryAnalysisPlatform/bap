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

  let check_invariant task s = match task with
    | Dest {dst} | Fall {dst} | Jump {src=dst} ->
      if not (Set.mem s.data dst)
      then Format.eprintf
          "Broken invariant: task %a is not retracted@\n%!"
          pp_task task

  let is_canceled s (Dest {dst} | Jump {src=dst} | Fall {dst}) =
    Set.mem s.data dst

  let rec cancel task s =
    Format.eprintf "canceling %a@\n" pp_task task;
    check_invariant task s;
    match task with
    | Dest {parent=None} -> s
    | Dest {parent=Some parent} | Fall {parent} | Jump {parent} ->
      match parent with
      | Dest {dst} | Fall {dst} as self ->
        Format.eprintf "canceling the parent %a, going up@\n" pp_task self;
        cancel parent (mark_data s dst)
      | Jump {src; dsts} -> match task with
        | Fall {delay=(Ready (Some _) | Delay)} ->
          Format.eprintf "canceling parent, as we're in its delay slot@\n";
          cancel parent (mark_data s src)
        | Fall _ when has_valid s dsts ->
          Format.eprintf "keeping parent jump alive@\n";
          s
        | Fall _ ->
          Format.eprintf "canceling the parent, as it has no valid dests@\n";
          cancel parent (mark_data s src)
        | Dest _ ->
          Format.eprintf "canceling the parent which produced invalid dest@\n";
          cancel parent (mark_data s src)
        | Jump _ ->
          Format.eprintf "Broken invariant: Jump to jump from %a@\n"
            Addr.pp src;
          assert false

  let rec step s =
    Format.eprintf "Step: %a => " pp_task s.curr;
    let () = match List.hd s.work with
      | None -> Format.eprintf "@\n"
      | Some t -> Format.eprintf "%a@\n" pp_task t in
    match s.work with
    | [] ->
      Format.eprintf "Scan is done, checking unsats...@\n";
      if Set.is_empty s.usat then begin
        Format.eprintf "everyone is sat, stopping@\n";
        {s with stop = true}
      end
      else begin
        Format.eprintf "Having %d more roots to work on@\n"
          (Set.length s.usat);
        step {s with work = init_work s.usat}
      end
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
            let is_data = Set.mem s.data dst in
            if is_data then
              Format.eprintf "Filtering out destination %a from %a@\n"
                Addr.pp dst pp_task jump;
            not is_data) in
        let dsts = {dsts with resolved} in
        Format.eprintf "registering %a as jump@\n" Addr.pp src;
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
    Format.eprintf "Failed on %a, canceling task %a@\n"
      Addr.pp addr pp_task s.curr;
    let s = mark_data s addr in
    let work = if is_visited s (Addr.succ addr) then s.work
      else Dest {dst=Addr.succ addr; parent=None} :: s.work in
    step @@ cancel s.curr {s with work}

  let stopped s =
    Format.eprintf "Hit the end, cancelling task %a@\n" pp_task s.curr;
    step @@ cancel s.curr @@ mark_data s s.addr

  let rec view s base ~empty ~ready =
    match Memory.view ~from:s.addr base with
    | Ok mem -> ready s mem
    | Error _ ->
      let s = match s.curr with
        | Fall _ as task ->
          Format.eprintf "cancelling an out-of-memory fall@\n";
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
  KB.provide Insn.Slot.name code (Dis.Insn.name insn) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let collect_dests arch mem insn =
  let width = Size.in_bits (Arch.addr_size arch) in
  let fall = Addr.to_bitvec (Addr.succ (Memory.max_addr mem)) in
  let init =
    {indirect = false; resolved = Set.empty (module Addr)} in
  new_insn arch mem insn >>= fun code ->
  KB.collect Insn.Slot.dests code >>= function
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
  KB.collect Insn.Slot.delay code >>| function
  | None -> 0
  | Some x -> x

let classify_mem mem =
  let empty = Set.empty (module Addr) in
  let base = Memory.min_addr mem in
  Seq.range 0 (Memory.length mem) |>
  KB.Seq.fold ~init:(empty,empty,empty) ~f:(fun (code,data,root) off ->
      let addr = Addr.(nsucc base off) in
      Theory.Label.for_addr (Addr.to_bitvec addr) >>= fun label ->
      KB.collect Insn.Slot.is_valid label >>= function
      | Some false -> KB.return (code,Set.add data addr,root)
      | r ->
        (* let code = if Option.is_none r then code
         *   else Set.add code addr in *)
        let code = Set.add code addr in
        KB.collect Insn.Slot.is_subroutine label >>| function
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
                 not dests.indirect then begin
                Format.eprintf "%a is a move@\n" Addr.pp s.addr;
                step d @@ Machine.moved s mem
              end
              else begin
                Format.eprintf "%a is a jump@\n" Addr.pp s.addr;
                delay arch mem insn >>= fun delay ->
                step d @@ Machine.jumped s mem dests delay
              end)
          ~invalid:(fun d _ s -> step d (Machine.failed s s.addr)))
    ~empty:KB.return

type t = {
  dis : (Dis.empty, Dis.empty) Dis.t;
  arch : arch;
  begs : Set.M(Addr).t;
  jmps : dsts Map.M(Addr).t;
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
  fun {Machine.begs; jmps; data; code} ->
  Format.eprintf "Finished scanning [%a:%d], got %d/%d@, %d->%d\n"
    Addr.pp (Memory.min_addr mem) (Memory.length mem)
    (Set.length code) (Set.length data)
    (Map.length jmps) (Set.length begs);
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

let execution_order stack =
  KB.List.fold stack ~init:[] ~f:(fun insns (mem,insn) ->
      KB.collect Insn.Slot.delay insn >>| function
      | None -> (mem,insn)::insns
      | Some d -> insert d (mem,insn) insns)

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
  let view ?len from mem = match Memory.view ?words:len ~from mem with
    | Ok mem -> mem
    | Error _ ->
      Format.eprintf "can't create [%a:%s] view over [%a,%a]@\n%!"
        Addr.pp from
        (Option.value_map len ~default:"None" ~f:string_of_int)
        Addr.pp (Memory.min_addr mem)
        Addr.pp (Memory.max_addr mem);
      assert false in
  let rec build cfg beg =
    if Set.mem data beg then KB.return (cfg,None)
    else follow beg >>= function
      | false -> KB.return (cfg,None)
      | true -> match Hashtbl.find blocks beg with
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
                  then begin
                    Format.eprintf "Block %a hit data %a from %a@\n"
                      Addr.pp beg Addr.pp next Addr.pp curr;
                    KB.return (curr,len,(mem,insn)::insns)
                  end
                  else
                  if Set.mem begs next || Map.mem jmps curr
                  then KB.return (curr,len,(mem,insn)::insns)
                  else Dis.jump s (view next base)
                      (next,len,(mem,insn)::insns))
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
