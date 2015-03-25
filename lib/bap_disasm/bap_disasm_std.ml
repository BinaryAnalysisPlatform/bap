(** The export module for the disasm library.
    Everything in this module will finish in the `Bap.Std` namespace.

    For intra-bap development consider to open this module, if you're
    outside this library or any dependent libraries.

*)
open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

include Bap_disasm_types
include Bap_disasm
include Bap_disasm_abi
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

(** include type definitions of the ABI  *)
module type Target = sig
  module CPU : CPU

  module ABI : sig
    include module type of Bap_disasm_abi_helpers
    (** registers given ABI under the given target   *)
    val register : abi_constructor -> unit
    (** [lift mem insn] lifts provided instruction to BIL.
        Usually you do not need to call this function directly, as
        [disassemble] function will do the lifting.
    *)

    (** creates a set of ABI for the provided symbol.
        Until [all] parameter is set to true the ABI will be
        disambiguated, using [choose] method. Only equally
        valid ABI are returned. *)
    val create :
      ?merge:(abi list -> abi) ->
      ?image:image ->
      ?sym:string -> mem -> Bap_disasm_block.t -> abi

  end

  val lift : mem -> ('a,'k) Basic.insn -> bil Or_error.t
end

(** {2 Lifted targets}
    All targets implement at least [Target] interface.
*)


(** ARM architecture  *)
module ARM  = struct
  include Bap_disasm_arm
  module ABI = struct
    include Bap_disasm_abi_helpers
    include Bap_disasm_arm_abi
  end
  include Bap_disasm_arm_lifter
end

module AMD64 = struct
  include Bap_disasm_x86_lifter.AMD64
  module ABI = struct
    include Bap_disasm_abi_helpers
    include Bap_disasm_amd64_abi
  end

end

module IA32 = struct
  include Bap_disasm_x86_lifter.IA32
  module ABI = struct
    include Bap_disasm_abi_helpers
    include Bap_disasm_ia32_abi
  end
end

module Stub = Bap_disasm_stub_lifter

let target_of_arch = function
  | #Arch.arm -> (module ARM : Target)
  | `x86_64 -> (module AMD64)
  | `x86 -> (module IA32)
  | _ -> (module Stub)
