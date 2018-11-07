open Core_kernel.Std
open Bap.Std
open Monads.Std
open Or_error
open Bap_llvm_ogre_types
open Bap_llvm_ogre_types.Scheme

module Elf = Bap_llvm_ogre_elf
module Coff = Bap_llvm_ogre_coff
module Macho = Bap_llvm_ogre_macho

let image_base = ref None


(** default image base for relocatable files *)
let relocatable_base = 0xC0000000L

module Fact(M : Monad.S) = struct
  include Ogre.Make(M)
  type 'a m = 'a M.t
end

module Dispatcher(M : Monad.S) = struct
  module Fact = Fact(M)
  open Fact.Syntax

  module type S = Bap_llvm_ogre_types.S with type 'a m := 'a Fact.t

  type typ = Elf | Coff | Macho | Unknown [@@deriving sexp]

  let filetype_of_string s =
    try
      typ_of_sexp (Sexp.of_string s)
    with _ -> Unknown

  let make_relocatable x =
    let module Target = (val x : Loader_target) in
    let module S = Target.Relocatable.Make(Fact) in
    if Option.is_none !image_base
    then image_base := Some relocatable_base;
    Fact.return (module S : S)

  let make x =
    Fact.require is_relocatable >>= fun is_rel ->
    if is_rel then make_relocatable x
    else
      let module Target = (val x : Loader_target) in
      Fact.return (module Target.Make(Fact) : S)

  let of_filetype =
    Fact.require file_type >>= fun s ->
    match filetype_of_string s with
    | Elf -> make (module Elf)
    | Coff -> make (module Coff)
    | Macho -> make (module Macho)
    | Unknown -> Fact.failf "file type is not supported" ()

end

module Make(M : Monad.S) = struct
  module Dispatcher = Dispatcher(M)
  include Dispatcher
  open Fact.Syntax

  let provide_base =
    Fact.require default_base_address >>= fun addr ->
    match !image_base with
    | None -> Fact.provide base_address addr
    | Some a -> Fact.provide base_address a

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
    S.symbols

  let image = Dispatcher.of_filetype >>= provide

end

module Loader = struct

  module Ogre_loader = Make(Monad.Ident)
  open Ogre_loader

  exception Llvm_loader_fail of int

  let _ = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)

  let to_image_doc doc =
    match Fact.exec image doc with
    | Ok doc -> Ok (Some doc)
    | Error er -> Error er

  let from_data data =
    try
      let doc = Bap_llvm_binary.bap_llvm_load data in
      Ogre.Doc.from_string doc >>= fun doc ->
      to_image_doc doc
    with Llvm_loader_fail n -> match n with
      | 1 -> Or_error.error_string "File corrupted"
      | 2 ->
         Or_error.error_string
"File format is not supported: expected executable, library or \
kernel module"
      | n -> Or_error.errorf "fail with unexpected error code %d" n

  let map_file path =
    let fd = Unix.(openfile path [O_RDONLY] 0o400) in
    try
      let size = Unix.((fstat fd).st_size) in
      let data = Bigstring.map_file ~shared:false fd size in
      Unix.close fd;
      Ok data
    with exn ->
      Unix.close fd;
      Or_error.errorf "unable to process file %s" path

  let from_file path =
    Or_error.(map_file path >>= from_data)

end

let init ?base () =
  image_base := base;
  Image.register_loader ~name:"llvm" (module Loader)
