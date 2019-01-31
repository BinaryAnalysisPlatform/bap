open Core_kernel
open Bap_integer_intf

module type Base = Base
module type S = S

module Make(Base : Base) = struct
  include Base
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ~-) = neg
  let (mod) = modulo
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = lshift
  let (lsr) = rshift
  let (asr) = arshift
end
