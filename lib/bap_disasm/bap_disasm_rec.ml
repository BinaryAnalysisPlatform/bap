open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bap_core_theory
open Bap_image_std

open KB.Syntax

module Driver = Bap_disasm_driver
module Brancher = Bap_disasm_brancher
module Rooter = Bap_disasm_rooter
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
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

type t = cfg

let create_insn basic prog =
  match basic with
  | None -> prog
  | Some insn ->
    let bil = Insn.bil prog in
    let prog' = Insn.of_basic ~bil insn in
    KB.Value.merge ~on_conflict:`drop_right prog prog'

let follows_after m1 m2 = Addr.equal
    (Addr.succ (Memory.max_addr m1))
    (Memory.min_addr m2)

let has_conditional_jump blk =
  let insn = Block.terminator blk in
  Insn.(is jump insn) &&
  Insn.(is conditional insn)

let global_cfg disasm =
  Driver.explore disasm
    ~init:Cfg.empty
    ~block:(fun mem insns ->
        Driver.execution_order insns >>=
        KB.List.filter_map ~f:(fun label ->
            KB.collect Basic.Insn.slot label >>= fun basic ->
            KB.collect Theory.Program.Semantics.slot label >>= fun s ->
            KB.collect Memory.slot label >>| function
            | None -> None
            | Some mem -> Some (mem,create_insn basic s)) >>|
        Block.create mem)
    ~node:(fun node cfg ->
        KB.return (Cfg.Node.insert node cfg))
    ~edge:(fun src dst g ->
        let k = if follows_after (Block.memory src) (Block.memory dst)
          then `Fall
          else if has_conditional_jump src
          then `Cond
          else `Jump in
        let edge = Cfg.Edge.create src dst k in
        KB.return @@ Cfg.Edge.insert edge g)



let result = Toplevel.var "cfg"

let extract self =
  Toplevel.put result self;
  Toplevel.get result

let with_unit =
  KB.Rule.(declare ~package:"bap" "unit-for-mem" |>
           dynamic ["arch"; "mem"] |>
           require Theory.Label.addr |>
           provide Theory.Label.unit |>
           comment "[Rec.{run,scan} arch mem] provides a unit for [mem]");
  fun arch mem ->
    let width = Size.in_bits (Arch.addr_size arch) in
    let is_little = Arch.endian arch = LittleEndian in
    let lower = Word.to_bitvec @@ Memory.min_addr mem
    and upper = Word.to_bitvec @@ Memory.max_addr mem in
    KB.promising Theory.Label.unit ~promise:(fun label ->
        KB.collect Theory.Label.addr label >>= function
        | Some p when Memory.contains mem @@ Word.create p width ->
          Theory.Unit.for_region ~lower ~upper >>= fun unit ->
          let (:=) slot value = KB.provide slot unit (Some value) in
          KB.List.sequence Theory.Unit.[
              Target.bits := width;
              Target.arch := Arch.to_string arch;
              Target.is_little_endian := is_little;
            ] >>= fun () ->
          KB.return (Some unit)
        | _ -> KB.return None)

let scan arch mem state =
  with_unit arch mem @@ fun () ->
  Driver.scan mem state

let run ?backend:_ ?(brancher=Brancher.empty) ?(rooter=Rooter.empty) arch mem =
  Brancher.provide brancher;
  Rooter.provide rooter;
  Result.return @@
  extract @@
  with_unit arch mem @@ fun () ->
  Driver.scan mem Driver.init >>= global_cfg

let cfg = ident
let errors _ = []
let create = KB.return
