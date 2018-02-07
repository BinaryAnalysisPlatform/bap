open Bap.Std
open Core_kernel.Std
include Self()
open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus taint propagatation engine.";
  `P
    "This is a deprecated control module, please primus-taint to
     control the Taint Analysis Framework."
]
let deprecated = "is deprecated, use the primus-taint plugin instead"



let enabled = flag "run" ~doc:deprecated
let don't_mark = flag "no-marks" ~doc:deprecated

(* deprecation doesn't work as expected with flags, so let's invent
 * something here... *)
let () = when_ready (fun {get=(!!)} ->
    if !!enabled || !!don't_mark then
      eprintf
        "Warning: the primus-propagate-taint plugin is deprecated, \
         use `primus-taint` plugin for fine control of the taint \
         analysis framework.\n%!")
