open Core_kernel.Std
open Graphlib.Std
open Bap.Std
open Cmdliner
include Self()

let man = [
  `S "DESCRIPTION";
  `P "Output symbol information. In the output file, each symbol is in format of:
     `(<symbol name> <symbol start address> <symbol end address>)', e.g.,
     `(malloc 0x11034 0x11038)'"
]
let info = Term.info name ~version ~doc ~man

let file : string option Term.t =
  let doc = "Dump symbols to the specified $(docv)" in
  Arg.(value & opt (some string) None & info ["file"] ~doc ~docv:"FILE")

let output oc syms =
  let sexp_of_sym x = [%sexp_of:string * int64 * int64] x in
  let output sym =
    Sexp.output_hum oc (sexp_of_sym sym);
    output_char oc '\n' in
  let word pro mem = ok_exn (Addr.to_int64 (pro mem)) in
  Symtab.to_sequence syms |> Seq.iter ~f:(fun (name,entry,cfg) ->
      Graphlib.reverse_postorder_traverse (module Graphs.Cfg)
        ~start:entry cfg |> Seq.iter ~f:(fun blk ->
            let mem = Block.memory blk in
            output (name,
                    word Memory.min_addr mem,
                    word Memory.max_addr mem)))

let main file proj =
  let syms = Project.symbols proj in
  match file with
  | Some name ->
    Out_channel.with_file name ~f:(fun oc -> output oc syms)
  | None -> output stdout syms

let () =
  let main = Term.(const main $file) in
  match Term.eval ~argv ~catch:false (main,info) with
  | `Ok main -> Project.register_pass' main
  | `Error _ -> exit 1
  | _ -> exit 0
