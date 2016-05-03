open Core_kernel.Std
open Bap_disasm_source_intf

module Factory(T : T) : Factory with type t = T.t
