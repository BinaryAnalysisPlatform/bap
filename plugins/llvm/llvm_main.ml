open Core_kernel.Std
open Bap.Std
open Cmdliner
include Self()

external init : unit -> int = "disasm_llvm_init_stub" "noalloc"

let x86_syntax =
  let names = ["att", "att"; "intel", "intel"] in
  let doc = sprintf "Choose style of code for x86 syntax between %s"
    @@ Arg.doc_alts_enum names in
  Arg.(value & opt (enum names) "att" & info ["x86-syntax"] ~doc)

let man = [
  `S "DESCRIPTION";
  `P "Uses LLVM-MC library to provide disassembler and file loader"
]

let tinfo = Term.info name ~version ~doc ~man

let () =
  match Term.eval ~argv Term.(x86_syntax,tinfo) with
  | `Ok syntax ->
    Unix.putenv "BAP_LLVM_OPTIONS" ("-x86-asm-syntax=" ^ syntax);
    let r = init () in
    if r < 0 then
      eprintf "LLVM initialization failed with error %d\n" r
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
