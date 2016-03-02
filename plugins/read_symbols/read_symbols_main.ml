open Core_kernel.Std
open Regular.Std
open Bap.Std
open Cmdliner

include Self()

let extract name img =
  let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
  let arch = Image.arch img in
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int64 ~width in
  In_channel.with_file name ~f:(fun ch ->
      Sexp.input_sexps ch |> List.map ~f:blk_of_sexp |>
      List.map ~f:(fun (n,s,e) -> n,addr s,addr e))

let symsfile : string option Term.t =
  let doc = "Use this file as symbols source" in
  Arg.(value & opt (some non_dir_file) None &
       info ["from";] ~doc ~docv:"SYMS")

module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  module Factory : sig
    val register : 'a source -> string -> ('a -> t option) -> unit
  end
end

let register syms =
  let name = "file" in
  let register (module T : Target) =
    T.Factory.register Source.Binary name (fun image ->
        let blocks = Seq.of_list (extract syms image) in
        Some (T.of_blocks blocks)) in
  register (module Rooter);
  register (module Symbolizer);
  register (module Reconstructor)

let man = [
  `S "DESCRIPTION";
  `P "Read symbol information from a file and provide rooter,
    symbolizer and a reconstructor, based on this information."
]

let info = Term.info name ~version ~doc ~man

let () =
  match Term.eval ~argv (symsfile,info) with
  | `Ok (Some file) -> register file
  | `Ok None -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
