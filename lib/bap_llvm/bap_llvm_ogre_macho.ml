open Core_kernel
open Bap.Std

open Bap_llvm_ogre_samples
open Bap_llvm_ogre_types.Scheme
open Bap_llvm_macho_scheme
open Image.Scheme

module Make(Fact : Ogre.S) = struct
  open Fact.Syntax

  let segments =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(
        select (from segment_cmd
                $ segment_cmd_flags
                $ virtual_segment_cmd)
          ~join:[[field name]])
      ~f:(fun (name, offset, size) (_,rwx) (_,addr,vsize)  ->
          name,offset,size,Int64.(base + addr),vsize,rwx) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, off, size, addr, vsize, (r,w,x)) ->
          Fact.provide segment addr vsize r w x >>= fun () ->
          Fact.provide named_region addr vsize name >>= fun () ->
          Fact.provide mapped addr size off)

  include Sections(Fact)
  include Symbols(Fact)
  include Regions(Fact)
end

module Relocatable = struct

  module Make(Fact : Ogre.S) = struct
    open Fact.Syntax

    let segments =
      Fact.require base_address >>= fun base ->
      Fact.foreach Ogre.Query.(begin
          select (from section_entry $ code_entry)
            ~join:[[field name];
                   [field size ~from:section_entry;
                    field size ~from:code_entry]]
        end)
        ~f:(fun hdr _ -> hdr) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun ((name,_,size,off)) ->
            let addr = Int64.(base + off) in
            Fact.provide segment addr size true true true >>= fun () ->
            Fact.provide mapped addr size off  >>= fun () ->
            Fact.provide named_region addr size name)

    include Relocatable_symbols(Fact)
    include Relocatable_sections(Fact)
    include Regions(Fact)
  end
end
