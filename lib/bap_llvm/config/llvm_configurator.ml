open Base
open Stdio

module C = Configurator.V1
module Buf = Caml.Buffer
module Arg = Caml.Arg
module Filename = Caml.Filename
open Llvm_config

let llvm_components = [
  "all-targets";
  "binaryformat";
  "core";
  "mc";
  "debuginfodwarf";
  "debuginfomsf";
  "debuginfopdb"
]

let llvm self opts =
  C.Process.run_capture_exn self llvm_config opts |>
  String.strip |>
  C.Flags.extract_blank_separated_words

let filename = ref ""
let args = [
  "-filename", Arg.Set_string filename, "NAME the name of the file";
]



let () = C.main ~args ~name:"bap-llvm" @@ fun self ->
  let linkmode =
    "--link-" ^
    (String.strip @@
     C.Process.run_capture_exn self llvm_config ["--shared-mode"]) in
  C.Flags.write_sexp "link.flags" @@ List.concat [
    llvm self [linkmode; "--ldflags"];
    llvm self ([linkmode; "--libs"] @ llvm_components);
    ["-lstdc++"; "-lcurses"; "-lzstd"];
  ];
  C.Flags.write_sexp "cxx.flags" @@ List.concat [
    ["-fPIC"];
    llvm self ["--cxxflags"];
  ];
  let version =
    String.strip @@
    C.Process.run_capture_exn self llvm_config ["--version"] in
  let src = !filename in
  let dst = Caml.Filename.chop_extension src in
  In_channel.with_file src ~f:(fun input ->
      Out_channel.with_file dst ~f:(fun output ->
          In_channel.iter_lines input ~f:(fun line ->
              let buf = Buffer.create (String.length line + 1) in
              Buf.add_substitute buf (function
                  | "llvm_version" -> version
                  | s -> s) line;
              Buf.add_char buf '\n';
              Buf.output_buffer output buf)))
