open Bap.Std
open Bap_primus.Std
include Self()

module Lisp_config = Primus_lisp_config

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "Installs and registers the Primus Lisp library. The library
      implements stdlib interface in Lisp. Only $(i,init) module is
      loaded automatically.";
    `P
      "The plugin also provides an interface through which it is
  possible to load new libraries and modules, or to load existing
  modules.";

    `S "SEE ALSO";
    `P "$(b,primus)(3) $(b,bap-run)(1)"
  ];

  let libs =
    Config.(param (list dir) ~doc:"paths to lisp libraries" "add") in

  let features =
    Config.(param (list string) ~doc:"load specified module" "load") in

  Config.when_ready (fun {Config.get=(!)} ->
      let paths = !libs @ [Lisp_config.library]  in
      let features = "init" :: !features in
      Primus.Lisp.init ~paths features)
