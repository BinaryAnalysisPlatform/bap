open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bap_core_theory
open Bap_image_std

open KB.Syntax

module Disasm = Bap_disasm_constrained_superset
module Brancher = Bap_disasm_brancher
module Rooter = Bap_disasm_rooter
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
module State = Bap_state
module Basic = Bap_disasm_basic

type full_insn = Basic.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type block = Block.t
type edge = Block.edge [@@deriving compare, sexp]
type jump = Block.jump [@@deriving compare, sexp]
type lifter = Bap_disasm_target_factory.lifter

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

type t = {cfg : Cfg.t}

let (>>=?) x f = x >>= function
  | None -> KB.return None
  | Some x -> f x

let provide_dests brancher =
  let init = Set.empty (module Theory.Label) in
  KB.promise Insn.Slot.dests @@ fun label ->
  KB.collect Memory.slot label >>=? fun mem ->
  KB.collect Basic.Insn.slot label >>=? fun insn ->
  Brancher.resolve brancher mem insn |>
  KB.List.fold ~init ~f:(fun dsts dst ->
      match dst with
      | Some addr,_ ->
        Theory.Label.for_addr (Word.to_bitvec addr) >>| fun dst ->
        Set.add dsts dst
      | None,_ -> KB.return dsts) >>|
  Option.some

let provide_roots rooter =
  let init = Set.empty (module Bitvec_order) in
  let roots =
    Rooter.roots rooter |>
    Seq.map ~f:Word.to_bitvec |>
    Seq.fold ~init ~f:Set.add in
  let promise prop =
    KB.promise prop @@ fun label ->
    KB.collect Theory.Label.addr label >>| function
    | None -> None
    | Some addr -> Option.some_if (Set.mem roots addr) true in
  promise Insn.Slot.is_valid;
  promise Insn.Slot.is_subroutine

let create_insn basic sema =
  let prog = Insn.create sema in
  match basic with
  | None -> prog
  | Some insn ->
    let bil = Insn.bil prog in
    let prog' = Insn.of_basic ~bil insn in
    KB.Value.merge ~on_conflict:`drop_right prog prog'

let follows_after m1 m2 = Addr.equal
    (Addr.succ (Memory.max_addr m1))
    (Memory.min_addr m2)

let disassemble brancher rooter dis mem =
  provide_dests brancher;
  provide_roots rooter;
  Disasm.scan dis mem >>=
  Disasm.explore
    ~init:Cfg.empty
    ~block:(fun mem insns ->
        Disasm.execution_order insns >>=
        KB.List.map ~f:(fun (mem,label) ->
            KB.collect Basic.Insn.slot label >>= fun basic ->
            KB.collect Theory.Program.Semantics.slot label >>| fun s ->
            mem,create_insn basic s) >>| Block.create mem)
    ~node:(fun node cfg ->
        KB.return (Cfg.Node.insert node cfg))
    ~edge:(fun src dst g ->
        let k = if follows_after (Block.memory src) (Block.memory dst)
          then `Fall
          else `Jump in
        let edge = Cfg.Edge.create src dst k in
        KB.return @@ Cfg.Edge.insert edge g)

type self = Reconstructor
let package = "bap.std.private"
let self = KB.Class.declare ~package "cfg-reconstructor" Reconstructor
let dom = KB.Domain.flat ~empty:Cfg.empty ~equal:Cfg.equal "cfg"
let cfg = KB.Class.property ~package self "cfg" dom


let run ?backend ?(brancher=Brancher.empty) ?(rooter=Rooter.empty) arch mem =
  match Disasm.create ?backend arch with
  | Error err -> Error err
  | Ok disasm ->
    let reconstructor =
      KB.Object.create self >>= fun obj ->
      disassemble brancher rooter disasm mem >>= fun r ->
      KB.provide cfg obj r >>| fun () ->
      obj in
    match Bap_state.run self reconstructor with
    | Ok r -> Ok {cfg = KB.Value.get cfg r}
    | Error _ -> Or_error.errorf "Some conflict"

let cfg t = t.cfg
let errors _ = []
