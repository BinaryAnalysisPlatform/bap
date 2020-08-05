open Core_kernel
open Bap.Std
open Monads.Std
open Or_error
open Bap_llvm_ogre_types
open Bap_llvm_ogre_types.Scheme

module Elf = Bap_llvm_ogre_elf
module Coff = Bap_llvm_ogre_coff
module Macho = Bap_llvm_ogre_macho

module Filename = Caml.Filename

module type Parameters = sig
  val image_base : int64 option
  val pdb_path   : string
end

module Fact(M : Monad.S) = struct
  include Ogre.Make(M)
  type 'a m = 'a M.t
end

module Ogre_loader(P : Parameters) = struct
  module Fact = Fact(Monad.Ident)
  module type S = Bap_llvm_ogre_types.S with type 'a m := 'a Fact.t
  open Fact.Syntax

  type typ = Elf | Coff | Macho | Unknown [@@deriving sexp]

  let filetype_of_string s =
    try
      typ_of_sexp (Sexp.of_string s)
    with _ -> Unknown

  let make x =
    let module Target = (val x : Loader_target) in
    Fact.require is_relocatable >>= fun is_rel ->
    if is_rel then
      let module S = Target.Relocatable.Make(Fact) in
      Fact.return (module S : S)
    else
      Fact.return (module Target.Make(Fact) : S)

  let of_filetype =
    Fact.require file_type >>= fun s ->
    match filetype_of_string s with
    | Elf -> make (module Elf)
    | Coff -> make (module Coff)
    | Macho -> make (module Macho)
    | Unknown -> Fact.failf "file type is not supported" ()

  let provide_base =
    Fact.require default_base_address >>= fun real ->
    match P.image_base with
    | None -> Fact.provide base_address real
    | Some base ->
      let base_bias = Int64.(base - real) in
      Fact.provide bias base_bias >>= fun () ->
      Fact.provide base_address base

  let provide_entry =
    Fact.require base_address >>= fun base ->
    Fact.require entry >>= fun addr ->
    Fact.provide entry_point Int64.(base + addr)

  let provide x =
    let module S = (val x : S) in
    provide_base >>= fun () ->
    provide_entry >>= fun () ->
    S.segments >>= fun () ->
    S.sections >>= fun () ->
    S.symbols  >>= fun () ->
    S.code_regions

  let image = of_filetype >>= provide

end

module Loader(P : Parameters) = struct

  module Ogre_loader = Ogre_loader(P)
  open Ogre_loader

  exception Llvm_loader_fail of int

  let _ = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)

  let pdb_file filename =
    if Sys.file_exists P.pdb_path then
      if Sys.is_directory P.pdb_path then
        let pdb_file = sprintf "%s.pdb"
            Filename.(remove_extension @@ basename filename) in
        Filename.concat P.pdb_path pdb_file
      else P.pdb_path
    else ""

  let to_image_doc doc =
    match Fact.exec image doc with
    | Ok doc -> Ok (Some doc)
    | Error er -> Error er

  let from_data filename data =
    try
      let doc = Bap_llvm_binary.bap_llvm_load data (pdb_file filename) in
      Ogre.Doc.from_string doc >>= fun doc ->
      to_image_doc doc
    with Llvm_loader_fail n -> match n with
      | 1 -> Or_error.error_string "File corrupted"
      | 2 -> Or_error.error_string "File format is not supported"
      | n -> Or_error.errorf "fail with unexpected error code %d" n

  let map_file path =
    let fd = Unix.(openfile path [O_RDONLY] 0o400) in
    try
      let data =
        (* Unix.map_file in 4.06; using the old location for compatibility *)
        Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout false [|-1|] in
      Unix.close fd;
      Ok (Bigarray.array1_of_genarray data)
    with exn ->
      Unix.close fd;
      Or_error.errorf "unable to process file %s: %s"
        path (Exn.to_string exn)
  [@@warning "-D"]

  let from_file path =
    Or_error.(map_file path >>= from_data path)

  let from_data data = from_data "" data

end

let init ?base ?(pdb_path=Sys.getcwd ()) () =
  Image.register_loader ~name:"llvm"
    (module Loader(struct
         let image_base = base
         let pdb_path = pdb_path
       end))
