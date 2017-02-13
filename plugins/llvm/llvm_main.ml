open Core_kernel.Std
open Bap.Std
include Self()

let () =
  match Bap_llvm_loader.init () with
  | Ok () -> ()
  | Error er -> eprintf "%s\n" (Error.to_string_hum er)

let disasm_init x86_syntax =
  match Bap_llvm_disasm.init ~x86_syntax () with
  | Ok () -> ()
  | Error er ->
    eprintf "%s\n" (Error.to_string_hum er)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Uses LLVM library to provide disassembler and file loader" ;
      `S "SEE ALSO";
      `P "$(b,bap-plugin-elf-loader)(1)";
    ] in
  let x86_syntax =
    let names = ["att", "att"; "intel", "intel"] in
    let doc = sprintf "Choose style of code for x86 syntax between %s"
      @@ Config.doc_enum names in
    Config.(param (enum names) ~default:"att" "x86-syntax" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      match !x86_syntax with
      | "att" | "intel" as s ->
        let syn = Bap_llvm_disasm.x86_syntax_of_sexp (Sexp.of_string s) in
        disasm_init syn
      | s -> eprintf "unknown x86-asm-syntax: %s" s)
