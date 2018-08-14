open Core_kernel.Std
open Bap.Std

include Self()

let () =
  Rel_brancher.init ();
  Rel_solver.init ();
  Config.manpage [
    `S "SYNOPSIS";
    `Pre "
      $(b,--brancher=relocatable)
     ";
    `S "DESCRIPTION";
    `P "Provides a brancher for relocatable files.";
    `P "Usually, jumps in a relocatable program do not have a direct
     static references to a destination but rather some bogus fixup.
     The default brancher is unable to infer the destination, as it
     is not encoded in a program itself, but in meta data of a file.
     The relocatable brancher will use OGRE to query for relocations
     and provide correct direct references to static code. For the
     external code references it will ouput a reference to the NULL
     address (along with the fallthrough address)";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-llvm)(1) code";
  ];
  Config.when_ready (fun _ -> ())
