open Core_kernel
open Bap.Std
open Monads.Std
open Or_error

module Unix = Caml_unix

module Primitive = struct
  (** [bap_llvm_load data pdb_path] analyzes [data] and builds an llvm
      specification of the discovered meta-data.

      - [pdb_path] is a path to PE debugging database, could be empty.
  *)
  external llvm_load : Bigstring.t -> string -> string =
    "bap_llvm_load_stub"

  exception Llvm_loader_fail of int

  let () = Callback.register_exception
      "Llvm_loader_fail" (Llvm_loader_fail 0)
end

module LLVM = struct
  open Ogre.Type
  open Image.Scheme

  let flag = "flag" %: bool

  (** loadable entry *)
  let ld = "ld" %: bool

  (** pure symbol's value, without interpretation *)
  let value = "value" %: int

  let at = "at" %: int

  (** entry point  *)
  let entry_point () =
    Ogre.declare ~name:"llvm:entry-point" (scheme addr) ident

  let base_address () =
    Ogre.declare ~name:"llvm:base-address" (scheme addr) ident

  (** (llvm:relocation from to). *)
  let relocation () =
    Ogre.declare ~name:"llvm:relocation" (scheme at $ addr) Tuple.T2.create

  (** an external symbols with the given name is referenced ad *)
  let name_reference () =
    Ogre.declare ~name:"llvm:name-reference" (scheme at $ name) Tuple.T2.create

  let section_entry () =
    Ogre.declare ~name:"llvm:section-entry" (scheme name $ addr $ size $ off)
      (fun name addr size off -> name, addr, size, off)

  (** named entry that contains code *)
  let code_entry () =
    Ogre.declare ~name:"llvm:code-entry" (scheme name $ off $ size) Tuple.T3.create

  (** symbol *)
  let symbol_entry () =
    Ogre.declare ~name:"llvm:symbol-entry"
      (scheme name $ addr $ size $ off $ value)
      (fun name addr size off value -> name, addr, size, off, value)

  let section_flags () =
    Ogre.declare ~name:"llvm:section-flags"
      (scheme name $ readable $ writable $ executable)
      (fun name r w x -> name, (r,w,x))

  (** macho segment command *)
  let segment_cmd () =
    Ogre.declare ~name:"llvm:segment-command" (scheme name $ off $ size)
      Tuple.T3.create

  let segment_cmd_flags () =
    Ogre.declare ~name:"llvm:segment-command-flags"
      (scheme name $ readable $ writable $ executable)
      (fun name r w x -> name, (r,w,x))

  let virtual_segment_cmd () =
    Ogre.declare ~name:"llvm:virtual-segment-command"
      (scheme name $ addr $ size) Tuple.T3.create

  (** macho symbol that doesn't belong to any section *)
  let macho_symbol () =
    Ogre.declare ~name:"llvm:macho-symbol" (scheme name $ value)
      Tuple.T2.create

  (** elf program header as it is in file *)
  let elf_program_header () = Ogre.declare "llvm:elf-program-header"
      (scheme name $ off $ size) Tuple.T3.create

  (** elf program header as it is in memory *)
  let elf_virtual_program_header () =
    Ogre.declare "llvm:elf-virtual-program-header"
      (scheme name $ addr $ size) Tuple.T3.create

  (** elf program header flags *)
  let elf_program_header_flags () =
    Ogre.declare "llvm:elf-program-header-flags"
      (scheme name $ ld $ readable $ writable $ executable)
      (fun name ld r w x -> name,ld,r,w,x)

  (** coff section in memory *)
  let coff_virtual_section_header () =
    Ogre.declare ~name:"llvm:coff-virtual-section-header"
      (scheme name $ addr $ size) Tuple.T3.create
end

module Filename = Caml.Filename

open Image.Scheme
open Ogre.Syntax

let foreach query f =
  Ogre.foreach query ~f >>=
  Ogre.Seq.iter ~f:Ogre.sequence

let foreach_by_name query f =
  foreach Ogre.Query.(select query ~join:[[field name]]) f

let iter_rows fieldname f =
  Ogre.require bias >>= fun bias ->
  Ogre.collect Ogre.Query.(select (from fieldname)) >>=
  Ogre.Seq.iter ~f:(fun entry -> Ogre.sequence (f bias entry))

let provide_if cond code =
  if cond then code else []

let provide_base_and_bias new_base =
  Ogre.require LLVM.base_address >>= fun real ->
  Ogre.sequence @@ match new_base with
  | None -> [
      Ogre.provide bias 0L;
      Ogre.provide base_address real;
    ]
  | Some base -> [
      Ogre.provide bias Int64.(base - real);
      Ogre.provide base_address base;
    ]

let provide_entry =
  Ogre.require bias >>= fun bias ->
  Ogre.require LLVM.entry_point >>= fun addr ->
  Ogre.provide entry_point Int64.(addr + bias)

let provide_symbols =
  iter_rows LLVM.symbol_entry @@ fun bias (name, addr, size, off, value) ->
  let addr = Int64.(addr + bias) in
  provide_if Poly.(size > 0L) [
    Ogre.provide symbol_chunk addr size addr;
    Ogre.request LLVM.code_entry ~that:(fun (n,o,s) ->
        Poly.(o = off && n = name && s = size)) >>= fun entry ->
    if Option.is_some entry then Ogre.provide code_start addr
    else Ogre.return ()
  ] @ [
    Ogre.provide symbol_value addr value;
    Ogre.provide named_symbol addr name;
  ]

let provide_generic_sections =
  iter_rows LLVM.section_entry @@ fun bias (name, addr, size, _) ->
  let addr = Int64.(addr + bias) in [
    Ogre.provide section addr size;
    Ogre.provide named_region addr size name
  ]

let provide_relocations =
  iter_rows LLVM.relocation @@ fun bias (addr, dest) -> [
    Ogre.provide relocation Int64.(addr + bias) Int64.(dest + bias)
  ]

let provide_name_references =
  iter_rows LLVM.name_reference @@ fun bias (addr, name) -> [
    Ogre.provide external_reference Int64.(addr + bias) name
  ]

let provide_generic_code_regions =
  foreach Ogre.Query.(begin
      select (from named_region $ LLVM.code_entry)
        ~join:[[field name];
               [field size ~from:named_region;
                field size ~from:LLVM.code_entry]]
    end) @@ fun {addr; size;} (_,off,_) -> [
    Ogre.provide code_region addr size off
  ]


let provide_macho_segments =
  Ogre.require bias >>= fun bias ->
  foreach_by_name Ogre.Query.(from
                                LLVM.segment_cmd
                              $ LLVM.segment_cmd_flags
                              $ LLVM.virtual_segment_cmd) @@
  fun (name, off, size) (_,(r,w,x)) (_,addr,vsize) ->
  let addr =  Int64.(bias + addr) in [
    Ogre.provide segment addr vsize r w x;
    Ogre.provide named_region addr vsize name;
    Ogre.provide mapped addr size off;
  ]

let provide_macho_segmentation = [
  provide_macho_segments;
  provide_generic_sections;
  provide_generic_code_regions;
]

let derive_code_regions_of_segments =
  Ogre.require bias >>= fun bias ->
  foreach_by_name Ogre.Query.(from
                                LLVM.elf_program_header
                              $ LLVM.elf_virtual_program_header
                              $ LLVM.elf_program_header_flags) @@
  fun (_,off,size) (_, addr, _) (_,_,_,_,x) ->
  provide_if x [
    Ogre.provide code_region Int64.(addr + bias) size off
  ]

let map_segments_to_sections =
  iter_rows LLVM.elf_virtual_program_header @@ fun bias (_,addr,size) -> [
    Ogre.provide section Int64.(addr + bias) size
  ]

let map_sections_to_segments =
  Ogre.require bias >>= fun bias ->
  foreach_by_name Ogre.Query.(from
                                LLVM.section_entry
                              $ LLVM.section_flags) @@
  fun (name,addr,size,off) (_,(r,w,x)) ->
  let addr = Int64.(addr + bias) in [
    Ogre.provide segment addr size r w x >>= fun () ->
    Ogre.provide mapped addr size off  >>= fun () ->
    Ogre.provide named_region addr size name
  ]

let use_when_missing missing_entry derivers =
  Ogre.collect Ogre.Query.(select (from missing_entry)) >>= fun entries ->
  if (Seq.is_empty entries)
  then Ogre.sequence derivers
  else Ogre.return ()

let derive_missing_sections = use_when_missing LLVM.section_entry [
    derive_code_regions_of_segments;
    map_segments_to_sections;
  ]

let derive_missing_segments = use_when_missing segment [
    map_sections_to_segments;
  ]

let derive_missing = Ogre.sequence [
    derive_missing_sections;
    derive_missing_segments;
  ]

let provide_elf_segments =
  Ogre.require bias >>= fun bias ->
  foreach_by_name Ogre.Query.(from
                                LLVM.elf_program_header
                              $ LLVM.elf_virtual_program_header
                              $ LLVM.elf_program_header_flags) @@
  fun (name,off,size) (_, addr, vsize) (_,ld,r,w,x) ->
  let addr = Int64.(addr + bias) in
  provide_if ld [
    Ogre.provide segment addr vsize r w x;
    Ogre.provide mapped addr size off;
    Ogre.provide named_region addr vsize name;
  ]

let provide_elf_segmentation = [
  provide_generic_sections;
  provide_elf_segments;
  provide_generic_code_regions;
  derive_missing;
]

let provide_coff_segmentation = [
  Ogre.require bias >>= fun bias ->
  foreach_by_name Ogre.Query.(from
                                LLVM.section_entry
                              $ LLVM.coff_virtual_section_header
                              $ LLVM.section_flags) @@
  fun (name, _, size, start) (_,addr,vsize) (_,(r,w,x)) ->
  let addr = Int64.(addr + bias) in [
    Ogre.provide segment addr vsize r w x;
    Ogre.provide mapped addr size start;
    Ogre.provide section addr vsize;
    Ogre.provide named_region addr vsize name;
  ] @ provide_if x [
      Ogre.provide code_region addr vsize start;
    ]

]

let overload file_type info =
  Ogre.require format >>= fun file ->
  Ogre.sequence @@
  provide_if (String.equal file file_type) info

(** translates llvm-specific specification into the image specification  *)
let translate user_base =
  Ogre.sequence [
    provide_base_and_bias user_base;
    provide_entry;
    Ogre.sequence [
      overload "elf"   provide_elf_segmentation;
      overload "coff"  provide_coff_segmentation;
      overload "macho" provide_macho_segmentation;
    ];
    provide_symbols;
    provide_relocations;
    provide_name_references;
  ]

let pdb_path ~pdb filename =
  if Sys.file_exists pdb then
    if Sys.is_directory pdb then
      let pdb_file = sprintf "%s.pdb"
          Filename.(remove_extension @@ basename filename) in
      Filename.concat pdb pdb_file
    else pdb
  else ""

let translate_to_image_spec base doc =
  match Ogre.exec (translate base) doc with
  | Ok doc -> Ok (Some doc)
  | Error er -> Error er

let load_doc ~pdb filename data =
  try Ok (Primitive.llvm_load data (pdb_path ~pdb filename))
  with Primitive.Llvm_loader_fail n -> match n with
    | 1 -> Or_error.error_string "File corrupted"
    | 2 -> Or_error.error_string "File format is not supported"
    | n -> Or_error.errorf "fail with unexpected error code %d" n

let from_data ~base ~pdb filename data =
  let open Or_error.Monad_infix in
  load_doc ~pdb filename data >>=
  Ogre.Doc.from_string >>=
  translate_to_image_spec base

let map_file path =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  try
    let data =
      Unix.map_file
        fd Bigarray.char Bigarray.c_layout false [|-1|] in
    Unix.close fd;
    Ok (Bigarray.array1_of_genarray data)
  with exn ->
    Unix.close fd;
    Or_error.errorf "unable to process file %s: %s"
      path (Exn.to_string exn)
[@@warning "-D"]

let from_file ~base ~pdb path =
  Or_error.(map_file path >>= from_data ~base ~pdb path)

let init ?base ?(pdb_path=Sys.getcwd ()) () =
  Image.register_loader ~name:"llvm" (module struct
    let from_file = from_file ~base ~pdb:pdb_path
    let from_data = from_data ~base ~pdb:pdb_path ""
  end);
  Ok ()
