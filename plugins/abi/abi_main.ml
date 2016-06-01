open Core_kernel.Std
open Bap.Std

let () = Project.register_pass ~autorun:true Bap_abi.pass
