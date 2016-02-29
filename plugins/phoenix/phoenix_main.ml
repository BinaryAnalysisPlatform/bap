open Core_kernel.Std
open Bap.Std
open Cmdliner
open Format

include Self()


let run options project =
  let arch = Project.arch project in
  let module Target = (val target_of_arch arch) in
  let module Phoenix = Phoenix_output.Make(struct
      let project = project
      let options = options
      module Target = Target
    end) in
  let dest = Phoenix.store () in
  printf "Stored data to %s@." dest

let cfg_format : 'a list Term.t =
  Arg.(value & vflag_all [`with_name] [
      `with_name, info ["labels-with-name"]
        ~doc: "Put block name on graph labels";
      `with_asm, info ["labels-with-asm"]
        ~doc:"Put assembler instructions on graph labels";
      `with_bil, info ["labels-with-bil"]
        ~doc:"Put bil instructions on graph labels";
    ])

let output_folder : _ Term.t =
  let doc = "Output data into the specified folder" in
  let default = "phoenix" in
  Arg.(value & opt string default & info ["output-folder"] ~doc)


let no_resolve : bool Term.t =
  let doc = "Do not resolve addresses to symbolic names" in
  Arg.(value & flag & info ["no-resolve"] ~doc)

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

let create
    a b c d e f g = Phoenix_options.Fields.create
    a b c d e f g


let options = Term.(const create
                    $output_folder
                    $cfg_format
                    $no_resolve
                    $keep_alive
                    $no_inline
                    $keep_consts
                    $no_optimizations)

let man = [
  `S "DESCRIPTION";
  `P "Output information about processed binary in a human readable
      format. This will emit CFG for each format in dot format. It
      will also store BIL and ASM code in html format.Output folder
      can be optionally specified. If omitted, the basename of the
      target file will be used as a directory name."
]

let info = Term.info ~man ~doc ~version name

let () =
  match Term.eval ~argv ~catch:false (options,info) with
  | `Ok options -> Project.register_pass' (run options)
  | `Version | `Help -> exit 0
  | `Error _ -> exit 1
