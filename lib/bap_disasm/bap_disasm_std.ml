include Bap_disasm_types
include Bap_disasm
module Insn    = Bap_disasm_insn
module Block   = Bap_disasm_block


(** {4 Expert interface to disassembler}
    This interface is rather complicated, and is built around to
    implementations of the disassembler [Basic] and [Recursive].
    [Basic] provides an efficient (and very lazy) linear sweep,
    driven in a continuation passing style. On top of the [Basic]
    the [Recursive] disassembler is built, that reconstructs the
    control flow graph, and represents the latter as a table of
    blocks. *)
module Disasm_expert = struct
  module Basic = Bap_disasm_basic
  module Recursive = Bap_disasm_rec
  module Kind = Bap_insn_kind
  module Insn = Bap_disasm_basic.Insn
  module Block = Bap_disasm_rec.Block
end
