open Bap.Std
open Primus.Std
include Self()

module Stdlib = Primus_lisp_library_config

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
      let libs = Stdlib.library :: !libs in
      let features = "init" :: !features in
      let module Main(Machine : Machine.S) = struct
        open Machine.Syntax
        module Lisp = Lisp.Machine.Make(Machine)
        let init () =
          Machine.List.iter libs ~f:Lisp.add_directory >>= fun () ->
          Machine.List.iter features ~f:Lisp.load_feature
      end in
  Machine.add_component (module Main))
