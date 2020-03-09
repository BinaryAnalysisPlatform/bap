open Bap.Std
open Bap_primus.Std
include Self()

let init _ =
  Primus.Machine.add_component
    (module Primus_x86_loader.InitializeFlags)
  [@warning "-D"];
  Primus.Components.register_generic
    ~package:"bap" "x86-flag-initializer"
    (module Primus_x86_loader.InitializeFlags)
    ~desc:"Intializes x86 flags to zero.";

  Primus.Machine.add_component
    (module Primus_x86_loader.SetupPLT)
  [@warning "-D"];
  Primus.Components.register_generic
    ~package:"bap" "x86-setup-plt"
    (module Primus_x86_loader.SetupPLT)
    ~desc:"Sets up PLT entries."
;;

Config.manpage [
  `S "DESCRIPTION";
  `P
    "Performs the x86 target specific setup. So far it just initializes
  all flag registers to zero."
];;

Config.when_ready init
