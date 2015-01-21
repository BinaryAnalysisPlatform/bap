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
        ~doc: "put block name on graph labels";
      `with_asm, info ["labels-with-asm"]
        ~doc:"put assembler instructions on graph labels";
      `with_bil, info ["labels-with-bil"]
        ~doc:"put bil instructions on graph labels";
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


let target_format : _ Term.t =
  let vals = ["numeric", `numeric; "symbolic", `symbolic] in
  let doc =
    sprintf "Set jump destinations format in BIL to %s"
      (Arg.doc_alts_enum vals) in
  Arg.(value & opt (enum vals) `symbolic &
       info ["dests"] ~doc)

let keep_alive : bool Term.t =
  let doc = "Keep alive dead BIL code" in
  Arg.(value & flag & info ["keep-alive"] ~doc)


let create
    a b c d e f g h = Options.Fields.create
    a b c d e f g h

let program =
  let doc = "Disassemble binary" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) disassembles $(i,FILE) and outputs results in a
          user ajustable form.";
    `S "SYMBOL TABLES";
    `P "Although $(tname) will try to extract symbols from \
        the binary file, you can also provide symbols externally, \
        using `-s' option. The argument to this option must be a \
        file containing s-expressions. Each s expression is list \
        of three atoms: name, starting address, ending address (i.e. \
        an address of the byte that immidiately follows the last \
        byte of the symbol).";
    `S "BUGS";
    `P "Report bugs to \
        https://github.com/BinaryAnalysisPlatform/bap/issues";
    `S "SEE ALSO"; `P "$(b,bap-mc)(1)"
  ] in
  Term.(pure create
        $filename $symsfile $cfg_format
        $output_phoenix $output_dump $demangle
        $target_format $keep_alive),
  Term.info "bap-objdump" ~version:"0.9.2" ~doc ~man

let parse () = match Term.eval program with
  | `Ok opts -> Ok opts
  | _ -> Or_error.errorf "no cmdline options provided"
