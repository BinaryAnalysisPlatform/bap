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

  let arch = Configuration.parameter
      ~doc:"Specifies the ISA of raw bytes"
      arch_t "arch"

  let entry_points = Configuration.parameter
      ~doc:"Address (or addresses) of entry points"
      Type.(list addr_t) "entry-point"

  let base_address = Configuration.parameter
      ~doc:"The address of the first byte"
      addr_t "base"

  let offset = Configuration.parameter
      ~doc:"The offset of the first byte"
      Type.int64 "offset"

  let length = Configuration.parameter
      ~doc:"The length of the code section"
      Type.(some int64) "length"
end

let doc_template = {|
(declare arch (name str))
(declare base-address (addr int))
(declare entry-point (addr int))
(declare mapped (addr int) (size int) (off int))
(declare code-region (addr int) (size int) (off int))
(declare named-region (addr int) (size int) (name str))
(declare segment (addr int) (size int) (r bool) (w bool) (x bool))
(declare section (addr int) (size int))
(declare code-start (addr int))

(arch $arch)
(base-address $base)
(entry-point $entry)
(mapped $base $length $offset)
(code-region $base $length $offset)
(named-region $base $length code)
(section $base $length)
(segment $base $length true false true)
|}

let register_loader ctxt =
  Image.register_loader ~name:"raw" (module struct
    let generate measure input =
      let options = Spec.[
          "arch", Arch.to_string (ctxt-->arch);
          "offset", Int64.to_string @@ ctxt-->offset;
          "base", Bitvec.to_string @@ ctxt-->base_address;
          "entry", begin match ctxt-->entry_points with
            | [] -> Bitvec.to_string @@ ctxt-->base_address;
            | x :: _ -> Bitvec.to_string x
          end;
          "length", Int64.to_string @@ match ctxt-->length with
          | None -> Int64.(measure input - (ctxt-->offset))
          | Some n -> n;
        ] |> String.Map.of_alist_exn in
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      doc_template |>
      Buffer.add_substitute buf (fun var ->
          match Map.find options var with
          | None -> invalid_argf "bug: missed a var: %S" var ()
          | Some v -> v);
      ctxt-->Spec.entry_points |>
      List.iter ~f:(Format.fprintf ppf "(code-start %a)@\n" Bitvec.pp);
      Format.pp_print_flush ppf ();
      Buffer.contents buf |>
      Ogre.Doc.from_string |>
      Or_error.ok_exn

    let length_of_file filename =
      let desc = Unix.openfile filename Unix.[O_RDONLY] 0o400 in
      let {Unix.LargeFile.st_size} = Unix.LargeFile.fstat desc in
      Unix.close desc;
      st_size

    let length_of_data str =
      Int64.of_int (Bigstring.length str)

    let from_file name =
      Or_error.try_with @@ fun () ->
      Some (generate length_of_file name)

    let from_data data =
      Or_error.try_with @@ fun () ->
      Some (generate length_of_data data)
  end)

let () = Extension.declare ~provides:["loader"] ~doc @@ fun ctxt ->
  Ok (register_loader ctxt)
