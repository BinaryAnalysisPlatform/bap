let doc = {|
# DESCRIPTION

Provides a loader for raw binaries. Raw binaries to not contain any
meta information or other headers, so this input should be provided
form the outside.
|}

open Bap.Std
open Bap_main
open Core_kernel
open Extension.Syntax

module Spec = struct
  open Extension

  let arch_t =
    let parse s = match Arch.of_string s with
      | None -> invalid_argf "Unknown architecture %s" s ()
      | Some a -> a in
    let default = `x86_64 in
    Type.define ~parse ~print:Arch.to_string default

  let addr_t =
    Type.define
      ~parse:Bitvec.of_string
      ~print:Bitvec.to_string
      Bitvec.zero

  let arch = Parameter.declare
      ~doc:"Specifies the ISA of raw bytes"
      arch_t "arch"

  let entry_points = Parameter.declare
      ~doc:"Address (or addresses) of entry points"
      Type.(list addr_t) "entry-point"

  let base_address = Parameter.declare
      ~doc:"The address of the first byte"
      addr_t "base"
end

let register_loader ctxt =
  let arch = ctxt-->Spec.arch in
  let to_word n = Addr.create n (Size.in_bits (Arch.addr_size arch)) in
  let entries = ctxt-->Spec.entry_points |> Seq.of_list |>
                Seq.map ~f:to_word |> Rooter.create
  and base = to_word @@ ctxt-->Spec.base_address in
  Rooter.provide entries;
  Project.Input.register_loader "raw" @@ fun filename ->
  Project.Input.binary ~base arch ~filename

let () = Extension.declare ~provides:["loader"] ~doc @@ fun ctxt ->
  Ok (register_loader ctxt)
