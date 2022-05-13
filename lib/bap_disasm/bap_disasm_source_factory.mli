open Core_kernel[@@warning "-D"]
open Bap_disasm_source_intf

module Factory : sig
  module type S = Factory
  module Make(T : T) : S with type t = T.t
end
