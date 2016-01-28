open Core_kernel.Std

module Std = struct
  type 'a printer = Format.formatter -> 'a -> unit
  include Bap_types.Std
  include Bap_image_std
  include Bap_disasm_std
  include Bap_sema.Std
  module Project = Bap_project
  type project = Project.t
  module Dwarf = Bap_dwarf
  module Elf = Bap_elf
  type elf = Elf.t
  module Signatures = Bap_signatures
  module Byteweight = Bap_byteweight
  include Graphlib.Std
  include Bap_trace_std
end

(* load internal plugins *)
let internal : (module Unit) list = [
  (module Bap_llvm);
  (module Bap_llvm_loader);
  (module Bap_native_loader);
]
