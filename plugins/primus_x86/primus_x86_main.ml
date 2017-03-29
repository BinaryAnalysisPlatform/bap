open Bap_primus.Std

let () = Primus.Machine.add_component (module Primus_x86_loader.Component)
