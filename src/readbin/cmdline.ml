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
  let doc = "Output data in a phoenix format. Output folder \
             can be optionally specified. If omitted, the \
             basename of the target file will be used as a \
             directory name." in
  let vopt = Some (Sys.getcwd ()) in
  Arg.(value & opt ~vopt (some string) None &
       info ["phoenix"; "p"] ~doc)

let output_dump : _ list Term.t =
  let values = [
    "asm", `with_asm;
    "bil", `with_bil;
  ] in
  let doc = sprintf
      "Print dump to standard output. Optional value \
       defines output format, and can be %s. You can \
       specify this parameter several times, if you \
       want both, for example."
    @@ Arg.doc_alts_enum values in
  Arg.(value & opt_all ~vopt:`with_asm (enum values) [] &
       info ["dump"; "d"] ~doc)

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
  Arg.(value & flag & info ["no-resolve"; "-n"] ~doc)

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


let create
    a b c d e f g h i k l m = Options.Fields.create
    a b c d e f g h i k l m
let program =
  let doc = "Disassemble binary" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) disassembles $(i,FILE) and outputs results in a
          user adjustable form.";
    `S "SYMBOL TABLES";
    `P "Although $(tname) will try to extract symbols from \
        the binary file, you can also provide symbols externally, \
        using `-s' option. The argument to this option must be a \
        file containing s-expressions. Each s expression is list \
        of three atoms: name, starting address, ending address (i.e. \
        an address of the byte that immediately follows the last \
        byte of the symbol).";
    `S "BUGS";
    `P "Report bugs to \
        https://github.com/BinaryAnalysisPlatform/bap/issues";
    `S "SEE ALSO"; `P "$(b,bap-mc)(1)"
  ] in
  Term.(pure create
        $filename $symsfile $cfg_format
        $output_phoenix $output_dump $demangle
        $no_resolve $keep_alive
        $no_inline $keep_consts $no_optimizations
        $binaryarch),
  Term.info "bap-objdump" ~version:"0.9.3" ~doc ~man

let parse () = match Term.eval program with
  | `Ok opts -> Ok opts
  | _ -> Or_error.errorf "no cmdline options provided"
