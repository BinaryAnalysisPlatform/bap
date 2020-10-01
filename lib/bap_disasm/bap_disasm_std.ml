open Core_kernel
open Bap_types.Std
open Bap_image_std

include Bap_disasm_types
include Bap_disasm
include Bap_disasm_target_factory

module Insn    = Bap_disasm_insn
module Block   = Bap_disasm_block

module Disasm_expert = struct
  type nonrec lifter = lifter
  module Basic = Bap_disasm_basic
  module Recursive = Bap_disasm_rec
  module Linear = Bap_disasm_linear_sweep
  module Kind = Bap_insn_kind
  module Insn = Bap_disasm_basic.Insn
  module Backend = Bap_disasm_backend_types
end

module Cfg = Bap_disasm_rec.Cfg
module Symtab  = Bap_disasm_symtab
module Source = Bap_disasm_source
module Rooter = Bap_disasm_rooter
module Symbolizer = Bap_disasm_symbolizer
module Brancher = Bap_disasm_brancher
module Reconstructor = Bap_disasm_reconstructor


type 'a source = 'a Source.t
type symtab = Symtab.t
type rooter = Rooter.t
type symbolizer = Symbolizer.t
type brancher = Brancher.t
type reconstructor = Reconstructor.t
type edge = Block.edge [@@deriving compare, sexp_poly]
type jump = Block.jump [@@deriving compare, sexp_poly]
