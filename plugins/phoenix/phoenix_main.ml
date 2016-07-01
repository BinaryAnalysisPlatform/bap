open Core_kernel.Std
open Bap.Std
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

(* Deprecated since, by design choice, this no longer maintains order
   of input. However, the [--label-with] will maintain order of
   input. *)
let dep_label_with x y =
  let name = sprintf "labels-with-%s" x in
  let doc = sprintf "Put %s on graph labels" y in
  Config.(flag ~deprecated:"Use --label-with=.. instead." name ~doc)

let with_name = dep_label_with "name" "block name"
let with_asm  = dep_label_with "asm"  "assembler instructions"
let with_bil  = dep_label_with "bil"  "bil instructions"

let labels_with : _ Config.param =
  let doc =
    "Put block name, assembler instructions, or bil instructions on
     graph labels using `name', `asm', or `bil' respectively. Can be
     specified as a list of multiple elements separated by commas." in
  let possibilities = [
    "name", `with_name;
    "asm",  `with_asm;
    "bil",  `with_bil;
  ] in
  Config.(param (list (enum possibilities)) ~default:[`with_name]
            "labels-with" ~doc)

let output_folder : string Config.param =
  let doc = "Output data into the specified folder" in
  Config.(param string ~default:"phoenix" "output-folder" ~doc)

let no_resolve : bool Config.param =
  let doc = "Do not resolve addresses to symbolic names" in
  Config.(flag "no-resolve" ~doc)

let keep_alive : bool Config.param =
  let doc = "Keep alive unused temporary variables" in
  Config.(flag "keep-alive" ~doc)

let no_inline : bool Config.param =
  let doc = "Disable inlining temporary variables" in
  Config.(flag "no-inline" ~doc)

let keep_consts : bool Config.param =
  let doc = "Disable constant folding" in
  Config.(flag "keep-const" ~doc)

let no_optimizations : bool Config.param =
  let doc = "Disable all kinds of optimizations" in
  Config.(flag "no-optimizations" ~doc)

let create
    a b c d e f g = Phoenix_options.Fields.create
    a b c d e f g

let man = [
  `S "DESCRIPTION";
  `P "Output information about processed binary in a human readable
      format. This will emit CFG for each format in dot format. It
      will also store BIL and ASM code in html format.Output folder
      can be optionally specified. If omitted, the basename of the
      target file will be used as a directory name."
]

let () =
  Config.manpage man;
  Config.when_ready (fun {Config.get=(!)} ->
      let cfg_format =
        let self = !labels_with in
        let self = if !with_name then `with_name :: self else self in
        let self = if !with_asm  then `with_asm  :: self else self in
        let self = if !with_bil  then `with_bil  :: self else self in
        self in
      let options = create
          !output_folder
          cfg_format
          !no_resolve
          !keep_alive
          !no_inline
          !keep_consts
          !no_optimizations in
      Project.register_pass' (run options))
