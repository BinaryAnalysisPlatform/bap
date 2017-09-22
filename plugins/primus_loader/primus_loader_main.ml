open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
include Self()



let init s b =
  let module Param = struct
    let stack_size = s
    let stack_base = b
  end in
  Primus.Machine.add_component (module Primus_loader_basic.Make(Param));;


Config.manpage [
  `S "DESCRIPTION";
  `P
    "Loads a binary and sets up the environment. This loader is
  generic and sets things in an architecture independent way. The loading
  process is finished by the backend (target specific) loaders if
  necessary.  The loader setups stack, prepares a callframe for the
  main function, fills it the environment and command line
  arguments. Finally it loads memory segments and allocates BSS
  sections. It also initializes several variables, that are usually
  defined by C programs - the $(b,environ) variable points to the list
  of environment variables, $(b,endp) points to the first address past
  the end of the last loaded segment, and $(b,brk) is the program
  break";

  `S "SEE ALSO";
  `P "$(b,bap-plugin-primus-x86)(1), $(b,brk)(2), $(b,end)(3)"
]

let stack_size =
  Config.(param int ~default:(8 * 1024 * 1024) "stack-size" ~doc)

let stack_base =
  Config.(param int64 ~default:Int64.(1L * 1024L * 1024L * 1024L)
            "stack-base"
            ~doc:"default address of the stack base")

let () = Config.when_ready (fun {Config.get=(!!)} ->
    init !!stack_size !!stack_base)
