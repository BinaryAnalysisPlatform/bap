open Core_kernel.Std
open Bap.Std
open Bap_llvm.Std
open Regular.Std

include Self()

let disasm_init x86_syntax =
  match init_disassembler ~x86_syntax () with
  | Ok () -> ()
  | Error er ->
    eprintf "%s\n" (Error.to_string_hum er)

let print_version () =
  printf "%s\n" llvm_version ;
  exit 0

let provider_name = "edu.cmu.ece.bap/llvm"


let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Uses LLVM library (statically linked in) to provide
          disassembling and file loading (parsing) services" ;
      `S "SEE ALSO";
      `P "$(b,bap-plugin-elf-loader)(1), $(b,bap-plugin-relocatable)(1)";
    ] in

  let x86_syntax =
    let names = [
      "att", `att;
      "intel", `intel
    ] in
    let doc = sprintf "Choose the x86 assembly syntax between %s" @@
      Config.doc_enum names in
    Config.(param (enum names) ~default:`att "x86-syntax" ~doc) in

  let base_addr =
    let doc =
      "Override image base address. If not specified and no base \
       address is provided by a binary itself, then defaults to  \
       0xC0000000." in

    Config.(param (some int64) ~default:None "base" ~doc) in
  let version =
    let doc ="Prints LLVM version and exits" in
    Config.(flag "version" ~doc) in

  let _disassembler = Service.(begin
      provide disassembler provider_name [
        parameter x86_syntax;
        parameter Config.input;
      ]
        ~desc:"an LLVM based disassembler"
    end) in

  let _loader = Service.(begin
      provide loader provider_name [
        parameter base_addr;
        parameter Config.input;
      ]
        ~desc:"an LLVM based program parser"
    end) in

  Config.when_ready (fun {Config.get=(!)} ->
      if !version then
        print_version();
      init_loader ?base:!base_addr ();
      disasm_init !x86_syntax)
