open Core_kernel
open Bap_core_theory
open KB.Syntax

let package = "dalvik"

let () = Project.Input.register_loader "dex" @@ fun filename ->
   let empty = Memmap.empty in
   let prog = prog_of_dex filename in
   Project.Input.create `dalvik filename ~code:empty ~data:empty
      ~finish:(fun proj -> Project.with_program prog)

