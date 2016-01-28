open Core_kernel.Std
open Cmdliner
open Format

let filename : string Term.t =
  let doc = "Input filename." in
  Arg.(required & pos 0 (some non_dir_file) None &
       info [] ~doc ~docv:"FILE")

let symsfile : string option Term.t =
  let doc = "Use this file as symbols source" in
  Arg.(value & opt (some non_dir_file) None &
       info ["syms"; "s"] ~doc ~docv:"SYMS")

let loader : string Term.t =
  let doc = "Backend name for an image loader" in
  Arg.(value & opt string "llvm" & info ["loader"] ~doc)

let cfg_format : 'a list Term.t =
  Arg.(value & vflag_all [`with_name] [
      `with_name, info ["labels-with-name"]
        ~doc: "Put block name on graph labels";
      `with_asm, info ["labels-with-asm"]
        ~doc:"Put assembler instructions on graph labels";
      `with_bil, info ["labels-with-bil"]
        ~doc:"Put bil instructions on graph labels";
    ])

let output_phoenix : _ Term.t =
  let doc = "Output information about processed binary in a human \
             readable format. This will emit CFG for each format in \
             dot format. It will also store BIL and ASM code in html \
             format.Output folder \
             can be optionally specified. If omitted, the \
             basename of the target file will be used as a \
             directory name." in
  let vopt = Some (Sys.getcwd ()) in
  Arg.(value & opt ~vopt (some string) None &
       info ["phoenix"; "output"] ~doc)

let output_dump : _ list Term.t =
  let values = [
    "asm", `with_asm;
    "bil", `with_bil;
    "bir", `with_bir;
  ] in
  let doc = sprintf
      "Print dump to standard output. Optional value \
       defines output format, and can be %s. You can \
       specify this parameter several times, if you \
       want both, for example."
    @@ Arg.doc_alts_enum values in
  Arg.(value & opt_all ~vopt:`with_asm (enum values) [] &
       info ["dump"; "d"] ~doc)

let dump_symbols : string option option Term.t =
  let doc = "Output symbol information. In the output file, each symbol is in format of:
  (<symbol name> <symbol start address> <symbol end address>), e.g.,
  (malloc@@GLIBC_2.4 0x11034 0x11038)" in
  Arg.(value & opt ~vopt:(Some None) (some (some string)) None & info
         ["dump-symbols"] ~doc)

let demangle : 'a option Term.t =
  let doc = "Demangle C++ symbols, using either internal \
             algorithm or a specified external tool, e.g. \
             c++filt." in
  let parse = function
    | "internal" -> `Ok `internal
    | name -> `Ok (`program name) in
  let printer fmt = function
    | `internal -> pp_print_string fmt "internal"
    | `program name -> pp_print_string fmt name in
  let spec = parse, printer in
  Arg.(value & opt ~vopt:(Some `internal) (some spec) None &
       info ["demangle"] ~doc)

let no_resolve : bool Term.t =
  let doc = "Do not resolve addresses to symbolic names" in
  Arg.(value & flag & info ["no-resolve"; "n"] ~doc)

let keep_alive : bool Term.t =
  let doc = "Keep alive unused temporary variables" in
  Arg.(value & flag & info ["keep-alive"] ~doc)

let no_inline : bool Term.t =
  let doc = "Disable inlining temporary variables" in
  Arg.(value & flag & info ["no-inline"] ~doc)

let keep_consts : bool Term.t =
  let doc = "Disable constant folding" in
  Arg.(value & flag & info ["keep-const"] ~doc)

let no_optimizations : bool Term.t =
  let doc = "Disable all kinds of optimizations" in
  Arg.(value & flag & info ["no-optimizations"] ~doc)

let binaryarch : _ Term.t =
  let doc =
    sprintf
      "Parse input file as raw binary with specified \
       architecture, e.g. x86, arm, etc." in
  Arg.(value & opt (some string) None &
       info ["binary"] ~doc)

let verbose : bool Term.t =
  let doc = "Print verbose output" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let bw_disable : bool Term.t =
  let doc = "Disable root finding with byteweight" in
  Arg.(value & flag & info ["no-byteweight"] ~doc)

let bw_length : int Term.t =
  let doc = "Maximum prefix length when byteweighting" in
  Arg.(value & opt int 16 & info ["byteweight-length"] ~doc)

let bw_threshold : float Term.t =
  let doc = "Minimum score for the function start" in
  Arg.(value & opt float 0.9 & info ["byteweight-threshold"] ~doc)

let print_symbols : _ list Term.t =
  let opts = [
    "name", `with_name;
    "addr", `with_addr;
    "size", `with_size;
  ] in
  let doc = sprintf
      "Print found symbols. Optional value \
       defines output format, and can be %s. You can \
       specify this parameter several times, if you \
       want both, for example."
    @@ Arg.doc_alts_enum opts in
  Arg.(value & opt_all ~vopt:`with_name (enum opts) [] &
       info ["print-symbols"; "p"] ~doc)

let use_ida : string option option Term.t =
  let doc = "Use IDA to extract symbols from file. \
             You can optionally provide path to IDA executable,\
             or executable name." in
  Arg.(value & opt ~vopt:(Some None)
         (some (some string)) None & info ["use-ida"] ~doc)

let sigsfile : string option Term.t =
  let doc = "Path to the signature file. No needed by default, \
             usually it is enough to run `bap-byteweight update'." in
  Arg.(value & opt (some non_dir_file) None & info ["sigs"] ~doc)

let emit_ida_script : string option Term.t =
  let doc = "Emit annotations to IDA based on project annotations to \
             the specified filename." in
  Arg.(value & opt (some string) None & info ["emit-ida-script"] ~doc)

let load : string list Term.t =
  let doc = "Load the specified plugin. Plugin must be compiled with \
             `bapbuild $(docv).plugin'. This option can be specified \
             several times with different names. " in
  Arg.(value & opt_all string [] & info ["l"] ~doc ~docv:"NAME")

let load_path : string list Term.t =
  let doc = "Add $(docv) to a set of search paths. Plugins specified \
             with `-l` flag will be searched in this paths, if they \
             are not found in the current folder or in a folder \
             specified by a `BAP_PLUGIN_PATH' environment variable" in
  Arg.(value & opt_all string [] &
       info ["load-path"; "L"] ~doc ~docv:"PATH")

let emit_attr : string list Term.t =
  let doc = "When printing IR emit attribute $(docv)" in
  Arg.(value & opt_all string [] &
       info ["emit-attr"; "A"] ~doc ~docv:"NAME")
