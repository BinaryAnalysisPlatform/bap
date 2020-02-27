open Bap.Std
open Bap_primus.Std
include Self()

let init _ =
  Primus.Components.register_generic
    ~package:"primus-x86" "flag-initializer"
    (module Primus_x86_loader.InitializeFlags)
    ~desc:"intializes x86 flags to zero";

  Primus.Components.register_generic
    ~package:"primus-x86" "setup-plt"
    (module Primus_x86_loader.SetupPLT)
    ~desc:"sets up PLT entries"
;;

Config.manpage [
  `S "DESCRIPTION";
  `P
    "Performs the x86 target specific setup. So far it just initializes
  all flag registers to zero."
];;

Config.when_ready init
