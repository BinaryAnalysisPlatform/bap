open Core_kernel
open Bap.Std
open Bap_llvm.Std
open Bap_main

include Self()

let disasm_init x86_syntax =
  match init_disassembler ~x86_syntax () with
  | Ok () -> ()
  | Error er ->
    eprintf "%s\n" (Error.to_string_hum er)

let print_version () =
  printf "%s\n" llvm_version ;
  exit 0

open Extension

let doc =
  "# DESCRIPTION

   Uses LLVM library to provide disassembler and file loader

   # SEE ALSO
   $(b,bap-plugin-elf-loader)(1), $(b,bap-plugin-relocatable)(1)"


let x86_syntax =
  let typ = Type.define
      ~parse:(fun s ->
          try
            x86_syntax_of_sexp (Sexp.of_string s)
          with _ -> raise (Invalid_argument s))
      ~print:(fun x -> Sexp.to_string (sexp_of_x86_syntax x))
      `att in
  let doc =
    "Choose style of code for x86 syntax between att and intel" in
  Configuration.parameter ~doc typ "x86-syntax"

let base_addr =
  let doc ="\
Replace image base address. If not set, a reasonable default corresponded
to a file type will be used. For example, for any executable file a
default image base is equal to lowest image virtual address.
For relocatable files a default image base is equal to 0xC0000000." in
  Configuration.parameter Type.(some int64) "base" ~doc

let pdb_path =
  let doc =
    "A path to a directory with pdb file OR a path to a PDB file.
     In the first case the file with the matching name of the target
     executable will be selected if present. The default is the path
     to a current working directory." in
  Configuration.parameter Type.(some string) "pdb-path" ~doc

let version =
  let doc ="Prints LLVM version and exits" in
  Configuration.flag "version" ~doc

let () =
  let open Syntax in
  Extension.declare @@ fun ctxt ->
  let (!) x = ctxt --> x in
  if !version then
    print_version();
  init_loader ?base:!base_addr ?pdb_path:!pdb_path ();
  disasm_init !x86_syntax;
  Ok ()
