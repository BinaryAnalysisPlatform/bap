open Bap.Std
open Bap_primus.Std
include Self()

let init _ = Primus_x86_loader.init () ;;

Config.manpage [
  `S "DESCRIPTION";
  `P
    "Performs the x86 target specific setup. So far it just initializes
  all flag registers to zero."
];;

Config.when_ready init
