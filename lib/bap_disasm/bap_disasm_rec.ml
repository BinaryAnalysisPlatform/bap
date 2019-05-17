open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bap_core_theory
open Bap_image_std

open KB.Syntax

module Disasm = Bap_disasm_speculative
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

type t = Disasm.t KB.t

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

let global_cfg disasm =
  Disasm.explore disasm
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


type self = CFG
let package = "bap.std.private"
let self = KB.Class.declare ~package "cfg" CFG
let dom = KB.Domain.flat ~empty:Cfg.empty ~equal:Cfg.equal "cfg"
let cfg = KB.Class.property ~package self "cfg" dom

let extract build disasm =
  let analysis =
    KB.Object.create self >>= fun obj ->
    disasm >>= build >>= fun r ->
    KB.provide cfg obj r >>| fun () ->
    obj in
  match Bap_state.run self analysis with
  | Ok r -> KB.Value.get cfg r
  | Error _ -> Cfg.empty


let run ?backend ?(brancher=Brancher.empty) ?(rooter=Rooter.empty) arch mem =
  match Disasm.create ?backend arch with
  | Error err -> Error err
  | Ok disasm ->
    Brancher.provide brancher;
    Rooter.provide rooter;
    Ok (Disasm.scan disasm mem)

let cfg = extract global_cfg
let errors _ = []
