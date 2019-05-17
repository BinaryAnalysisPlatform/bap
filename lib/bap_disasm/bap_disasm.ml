open Core_kernel
open Graphlib.Std
open Bap_types.Std

open Or_error

open Bap_disasm_types
open Image_internal_std

module Rec = Bap_disasm_rec
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
module Image = Bap_image
module Brancher = Bap_disasm_brancher
module Rooter = Bap_disasm_rooter
module Targets = Bap_disasm_target_factory

type block = Block.t [@@deriving compare, sexp_of]
type insn = Insn.t [@@deriving bin_io, compare, sexp]
type op = Op.t [@@deriving bin_io, compare, sexp]
type image = Image.t

type error = [
  | `Failed of Error.t
  | `Errors of error * error
  | Rec.error
] [@@deriving sexp_of]

type mem_state =
  | Failed of error                (** failed to decode anything    *)
  | Decoded of insn * error option (** decoded with optional errors *)
[@@deriving sexp_of]

type cfg = Rec.cfg [@@deriving compare]

type disasm = {
  cfg : cfg;
  err : Rec.error list;
}

type brancher = Brancher.t
type rooter = Rooter.t

let insns_of_blocks bs =
  Seq.(Rec.Cfg.nodes bs >>| Block.insns >>| of_list |> join)

let lifter_of_arch arch =
  let module Target = (val Targets.target_of_arch arch) in
  Target.lift


let empty = {
  cfg = Rec.Cfg.empty;
  err = [];
}


let of_rec d = {
  cfg = Rec.cfg d;
  err = Rec.errors d;
}


module Disasm = struct
  module Driver = Bap_disasm_driver

  type t = disasm
  type 'a disassembler = ?backend:string -> ?brancher:brancher -> ?rooter:rooter -> 'a

  let create  cfg = {
    cfg; err = [];
  }

  let insns t = insns_of_blocks t.cfg
  let cfg t = t.cfg
  let errors t = t.err

  let of_mem ?backend ?brancher ?rooter arch mem =
    Rec.run ?backend ?brancher ?rooter arch mem >>| of_rec

  let merge d1 d2 =
    let merge = Graphlib.union (module Rec.Cfg) in
    {cfg = merge d1.cfg d2.cfg; err = d1.err @ d2.err}

  let of_image ?backend ?brancher ?rooter image =
    let arch = Image.arch image in
    let rooter =
      Option.value rooter ~default:(Rooter.of_image image) in
    Table.foldi ~init:(return empty) (Image.segments image)
      ~f:(fun mem sec dis ->
          dis >>= fun dis ->
          if Image.Segment.is_executable sec then
            of_mem ?backend ?brancher ~rooter arch mem >>| merge dis
          else return dis)

  let of_file ?backend ?brancher ?rooter ?loader filename =
    Image.create ?backend:loader filename >>= fun (img,_) ->
    of_image ?backend ?brancher ?rooter img

  module With_exn = struct
    let of_mem ?backend ?brancher ?rooter arch mem =
      of_mem ?backend ?brancher ?rooter arch mem |> ok_exn
    let of_file ?backend ?brancher ?rooter ?loader filename =
      of_file ?backend ?brancher ?rooter ?loader filename |> ok_exn
    let of_image ?backend ?brancher ?rooter image =
      of_image ?backend ?brancher ?rooter image |> ok_exn
  end


  let insn  = Value.Tag.register (module Insn)
      ~name:"insn"
      ~uuid:"8e2a3998-bf07-4a52-a791-f74ea190630a"
end
