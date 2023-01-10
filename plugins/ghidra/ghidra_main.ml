open Core_kernel[@@warning "-D"]
open Bap_main

let default_paths =
  let (/) = Filename.concat in
  Extension.Configuration.[
    datadir / "ghidra";
    sysdatadir / "ghidra";
    "/usr/share/ghidra";
  ]

let show_targets = Extension.Configuration.flag "list-targets"
    ~aliases:["targets"]

let paths = Extension.Configuration.parameters
    Extension.Type.(list string) "path"

let () = Extension.declare
    ~doc:"provides loader, disassembler, and semantics using Ghidra Sleigh"
    ~provides:["disassembler"; "ghidra"; "lifter"; "semantics"; "p-code"]
  @@ fun ctxt ->
  let open Extension.Syntax in
  Bap_ghidra.init ()
    ~paths:(List.concat (ctxt-->paths) @ default_paths)
    ~print_targets:(ctxt-->show_targets);
  if (ctxt-->show_targets)
  then Error (Extension.Error.Exit_requested 0)
  else Ok ()
