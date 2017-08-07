open Core_kernel.Std
open Bap.Std
open Bap_llvm.Std
include Self()

let disasm_init x86_syntax =
  match Llvm_disasm.init ~x86_syntax () with
  | Ok () -> ()
  | Error er ->
    eprintf "%s\n" (Error.to_string_hum er)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Uses LLVM library to provide disassembler and file loader" ;
      `S "SEE ALSO";
      `P "$(b,bap-plugin-elf-loader)(1), $(b,bap-plugin-relocatable)(1)";
    ] in
  let x86_syntax =
    let names = ["att", "att"; "intel", "intel"] in
    let doc = sprintf "Choose style of code for x86 syntax between %s"
      @@ Config.doc_enum names in
    Config.(param (enum names) ~default:"att" "x86-syntax" ~doc) in
  let base_addr =
    let doc ="\
Replace image base address. If not set, a reasonable default corresponded
to a file type will be used. For example, for any executable file a
default image base is equal to lowest image virtual address.
For relocatable files a default image base is equal to 0xC0000000." in
    Config.(param (some int64) ~default:None "base" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      let () = Ogre_loader.init ?base:!base_addr () in
      match !x86_syntax with
      | "att" | "intel" as s ->
        let syn = Llvm_disasm.x86_syntax_of_sexp (Sexp.of_string s) in
        disasm_init syn
      | s -> eprintf "unknown x86-asm-syntax: %s" s)
