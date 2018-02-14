open Core_kernel.Std
open Bap.Std
open Cmdliner
open Format
open Bap_options

module type With_factory = sig
  module Factory : sig
    val list : unit -> string list
  end
end

let enum_processors (module T : With_factory) =
  T.Factory.list () |> List.map ~f:(fun x -> x,x)

let filename : string Term.t =
  let doc = "Input filename." in
  Arg.(required & pos 0 (some non_dir_file) None &
       info [] ~doc ~docv:"FILE")

let logdir : string option Term.t =
  let doc = "A folder for log files." in
  let env = Term.env_info ~doc "BAP_LOG_DIR" in
  Arg.(value & opt (some dir) None & info ["logdir"; "log-dir"] ~env ~doc)

let brancher () : string option Term.t =
  match enum_processors (module Brancher) with
  | [] | [_] -> Term.const None
  | names ->
    let doc = sprintf "Use specified brancher, should be %s" @@
      Arg.doc_alts_enum names in
    Arg.(value & opt (some (enum names)) None & info ["brancher"] ~doc)

let symbolizers () : string list Term.t =
  match enum_processors (module Symbolizer) with
  | [] -> Term.const []
  | [x,_] -> Term.const [x]
  | names ->
    let doc =
      sprintf "Use a specified symbolizer. If an option is specified
      several times, then symbolizers are merged. Possible values
      are: %s" @@ Arg.doc_alts_enum names in
    let default = List.map names ~f:fst in
    Arg.(value & opt_all (enum names) default & info ["symbolizer"] ~doc)

let rooters () : string list Term.t =
  match enum_processors (module Rooter) with
  | [] -> Term.const []
  | [x,_] -> Term.const [x]
  | names ->
    let doc = sprintf "Use a rooter with a given $(docv) . If an
    option is specified several times, then rooters are
    merged. If not specified, then all available rooters will be
    merged together. Possible values: %s." @@ Arg.doc_alts_enum names in
    let default = List.map names ~f:fst in
    Arg.(value & opt_all (enum names) default &
         info ["rooter"] ~doc ~docv:"NAME")

let reconstructor () : string option Term.t =
  match enum_processors (module Reconstructor) with
  | []  -> Term.const None
  | names ->
    let doc = "Use a specified reconstructor" in
    Arg.(value & opt (some (enum names)) None & info ["reconstructor"] ~doc)

let loader () : string Term.t =
  Project.Input.available_loaders () |>
  List.map ~f:(fun x -> x,x) |> function
  | []   -> Term.const "<no-loaders-available>"
  | [x,_] -> Term.const x
  | backends ->
    let doc = sprintf
        "Backend name for an image loader, should be %s" @@
      Arg.doc_alts_enum backends in
    Arg.(value & opt (enum backends) "llvm" & info ["loader"] ~doc)

let disassembler () : string Term.t =
  Disasm_expert.Basic.available_backends () |>
  List.map ~f:(fun x -> x,x) |> function
  | [] -> Term.const "<no-disassemblers-available>"
  | [x,_] -> Term.const x
  | backends ->
    let doc = sprintf
        "Disassembler backend, should be %s" @@
      Arg.doc_alts_enum backends in
    Arg.(value & opt (enum backends) "llvm" & info ["disassembler"] ~doc)

let rooters_mem = List.mem ~equal:[%compare.equal : string * string]

let symbols () : string list Term.t =
  let rooters = enum_processors (module Rooter) in
  let sybolzs = enum_processors (module Symbolizer) in
  match List.filter sybolzs ~f:(rooters_mem rooters) with
  | [] | [_] -> Term.const []
  | names ->
    let doc =
      sprintf "Use $(docv) as rooter and symbolizer. This is a
    shortcut for --rooter=$(docv) --symbolizer=$(docv). You can
    specify this option several times to mix different sources of
    information. Possible values %s" @@
      Arg.doc_alts_enum names in
    Arg.(value & opt_all (enum names) [] &
         info ["symbols"] ~doc ~docv:"NAME")

let list_formats, list_formats_doc =
  let doc =
    "Print detailed information about available project printers" in
  Arg.(value & flag & info ["list-formats"] ~doc), doc

let list_recipes =
  let doc = "Print all known recipes" in
  Arg.(value & opt ~vopt:(Some None) (some (some string)) None
       & info ["list-recipes"; "show-recipes"] ~doc)

let dump_formats () : Bap_fmt_spec.t list Term.t =
  let fmts = Project.available_writers () |>
             List.map ~f:(fun (n,_,_) -> n,n) in
  let parse = fst Bap_fmt_spec.t in
  match fmts with
  | [] -> Term.const []
  | (fmt,_) :: _ as fmts ->
    let vopt = match parse "bir" with
      | `Ok fmt -> fmt
      | `Error _ -> `stdout,fmt,None in
    let doc = sprintf
        "Print a project using the designated format to a specified
       destination. If format and destination is not specified, then a
       program will be printed in the IR form to the standard
       output. The argument consists of format specification, and an
       optional destination, separated from the format by a colon (:)
       symbol. The format specification, consists of format name and
       optional version number, separated from the name by a dash
       symbol, e.g., `<fmt>` or `<fmt>-<ver>`.  Acceptable format
       names are %s.  If version is omitted then the latest version of
       the given format will be used. If destination is not omitted,
       then data will to the specified destination, otherwise it will
       be printed to the standard output. The full argument grammar is
       `fmt ::= <fmt>[-<ver>][:<dst>]'. Examples: `bir', `bir-0.1`,
       `sexp-0.9:data.sexp`, etc. Use `--list-formats' command line
       option to get the detailed information about available formats
       and corresponding versions. This option can be specified
       several times, to dump the project is several formats (and
       possibly destinations) simultaneously." @@
      Arg.doc_alts_enum fmts in
    Arg.(value & opt_all ~vopt Bap_fmt_spec.t [] &
         info ["dump"; "d"] ~doc)

let source_type : source Term.t =
  let doc = "Defines a format of the input file. It can be either a
            `binary' that denotes a structured binary file in a a
            format that is supported by a loader. It can also be a
            `<arch>-code' (e.g., `arm-code', `x86-code') if the binary
            is a raw code for the given <arch>. If the $(docv) is
            `project`, then the input file must be project data
            serialized with some format available for project data
            type. The format can be encoded in the extension. If
            needed, then a version number can be specifed, separated
            from the format by a dash, e.g., `myproj.marshal`,
            `myproj.sexp-1.0', etc. If the format is not specified,
            then the default reader will be used." in
  Arg.(value & opt Bap_source_type.t `Binary & info ["source-type"]
         ~doc ~docv:"NAME")

let verbose : bool Term.t =
  let doc = "Print verbose output" in
  Arg.(value & flag & info ["verbose"] ~doc)

let load_doc =
  "Dynamically loads file $(i,PATH).plugin. A plugin must be compiled
   with $(b,bapbuild) tool using $(b,bapbuild PATH.plugin) command."
let load : string list Term.t =
  Arg.(value & opt_all string [] & info ["l"] ~doc:load_doc ~docv:"PATH")

let load_path_doc =
  "Add $(i,PATH) to a set of search paths. Plugins found in the search
  paths will be loaded automatically."

let load_path : string list Term.t =
  Arg.(value & opt_all string [] &
       info ["load-path"; "L"] ~doc:load_path_doc ~docv:"PATH")

let list_plugins, list_plugin_doc =
  let doc =
    "List all available plugins or list plugins that provide some
     features, e.g. --list-plugins=disassembler,lifter" in
  Arg.(value & opt ~vopt:(Some []) (some (list string)) None
       & info ["list-plugins"] ~doc), doc

let list_tags, list_tags_doc =
  let doc = "List all available plugin tags" in
  Arg.(value & flag & info ["list-tags"] ~doc), doc

let disable_plugin, disable_plugin_doc =
  let doc = "Don't load $(i,PLUGIN) automatically" in
  Arg.(value & opt_all string [] &
       info ["disable-plugin"] ~doc),doc

let no_auto_load, no_auto_load_doc =
  let doc = "Disable auto loading of plugins" in
  Arg.(value & flag & info ["disable-autoload"] ~doc), doc


let recipe =
  let doc = "Load the specified recipe" in
  Arg.(value & opt (some string) None & info ["recipe"] ~doc)

let loader_options = [
  "-l"; "-L"; "--load-path";
  "--list-plugins"; "--disable-plugin"; "--disable-autoload"
]

let common_loader_options = [
  `S "PLUGIN OPTIONS";
  `I ("$(b,-l) $(i,PATH)", load_doc);
  `I ("$(b,-L)$(i,PATH)", load_path_doc);
  `I ("$(b,--list-plugins)", list_plugin_doc);
  `I ("$(b,--list-tags)", list_tags_doc);
  `I ("$(b,--disable-plugin)", disable_plugin_doc);
  `I ("$(b,--disable-autoload)", no_auto_load_doc);
  `I ("$(b,--no-)$(i,PLUGIN)", disable_plugin_doc);
]


let options_for_passes = [
  `S "OPTIONS FOR PASSES";
  `I begin
    "$(b,--)$(i,PASS)",
    "Runs a program $(i,PASS). The $(i,PASS) should be registered in the
         system, usually by loading or installing corresponding
         plugin. DEPRECATED. This option will be removed in the near
         future. Instead, it is advisable to use the $(b,-p) option to
         specify passes."
  end;
  `I begin
    "$(b,--)$(i,PASS)-$(i,OPTION)",
    "Passes $(i,OPTION) to a $(i,PASS). The option will be passed as
         $(b,--OPTION). If $(i,OPTION) is followed by an argument
        (i.e., a token that doesn't start with a dash), then it will be
        also passed to the $(i,PASS). Note: it is assumed that all
        options to passes consumes and optional argument. Make sure,
        that your flags are followed by some option, otherwise the
        argument that follows the flag might be consumed by a $(i,PASS)"
  end;
]
