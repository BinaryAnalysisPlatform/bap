open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bap_core_theory
open Bap_image_std

open KB.Syntax

module Toplevel = Bap_toplevel
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

type t = Driver.state KB.t

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
          else `Jump in
        let edge = Cfg.Edge.create src dst k in
        KB.return @@ Cfg.Edge.insert edge g)



let result = Toplevel.var "cfg"

let extract build disasm =
  Toplevel.put result begin
    disasm >>= build
  end;
  Toplevel.get result


let provide_arch arch mem =
  let width = Size.in_bits (Arch.addr_size arch) in
  KB.promise Arch.slot @@ fun label ->
  KB.collect Theory.Label.addr label >>| function
  | None -> None
  | Some p ->
    let p = Word.create p width in
    if Memory.contains mem p then Some arch
    else None


let run ?backend ?(brancher=Brancher.empty) ?(rooter=Rooter.empty) arch mem =
  Brancher.provide brancher;
  Rooter.provide rooter;
  provide_arch arch mem;
  Ok (Driver.scan mem Driver.init)

let cfg = extract global_cfg
let errors _ = []
