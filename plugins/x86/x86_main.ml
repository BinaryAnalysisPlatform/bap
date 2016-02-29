open Bap.Std

module AMD64 = X86_lifter.AMD64
module IA32  = X86_lifter.IA32


let () =
  register_target `x86    (module IA32);
  register_target `x86_64 (module AMD64)
