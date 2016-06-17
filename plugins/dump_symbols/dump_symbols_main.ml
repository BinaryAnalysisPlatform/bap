open Core_kernel.Std
open Graphlib.Std
open Bap.Std
include Self()

let output oc syms =
  let output sym =
    Sexp.output_hum oc ([%sexp_of:string * int64 * int64] sym);
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
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Output symbol information. In the output file, each symbol
          is in format of:
          `(<symbol name> <symbol start address> <symbol end address>)', e.g.,
          `(malloc 0x11034 0x11038)'"
    ] in
  let file = Config.(param (some string) "file" ~default:None ~docv:"FILE"
                       ~doc:"Dump symbols to the specified $(docv)") in
  Config.when_ready (fun {Config.get=(!)} ->
      let main = main !file in
      Project.register_pass' main)
