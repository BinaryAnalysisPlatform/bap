open Core_kernel.Std
open Bap.Std
include Self()

let () = Config.manpage [
  `S "DESCRIPTION";
  `P "Run ABI specific passes. The passes are registered using
    Bap_abi.register_pass function, by target specific plugins,
    depending on the binary architecture and command line options,
    provided by a user.";
  `S "SEE ALSO";
  `P "$(b,bap-plugin-x86)(1), $(b,bap-plugin-arm)(1), $(b,bap-abi)(3)"
]

let () = Config.when_ready (fun _ -> Project.register_pass ~autorun:true Bap_abi.pass)
