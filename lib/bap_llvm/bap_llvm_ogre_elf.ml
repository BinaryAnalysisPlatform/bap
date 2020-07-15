open Core_kernel
open Bap.Std
open Monads.Std

open Image.Scheme
open Bap_llvm_ogre_samples
open Bap_llvm_ogre_types.Scheme
open Bap_llvm_elf_scheme

module Make(Fact : Ogre.S) = struct
  open Fact.Syntax

  let segments =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(begin
        select (from
                  program_header
                $ virtual_program_header
                $ program_header_flags)
          ~join:[[field name]]
      end)
      ~f:(fun hdr vhdr hdr_flags -> hdr, vhdr, hdr_flags) >>=
    Fact.Seq.iter
      ~f:(fun ((name,off,size), (_, relative_addr, vsize), (_,ld,r,w,x)) ->
          if ld then
            let addr = Int64.(relative_addr + base) in
            Fact.provide segment addr vsize r w x >>= fun () ->
            Fact.provide mapped addr size off >>= fun () ->
            Fact.provide named_region addr vsize name
          else Fact.return ())

  include Sections(Fact)
  include Symbols(Fact)
  include Code_regions(Fact)

  let code_of_segments =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(begin
        select (from
                  program_header
                $ virtual_program_header
                $ program_header_flags)
          ~join:[[field name]]
      end)
      ~f:(fun hdr vhdr hdr_flags -> hdr, vhdr, hdr_flags) >>=
    Fact.Seq.iter
      ~f:(fun ((_,off,size), (_, relative_addr, _), (_,_,_,_,x)) ->
          if x then
            let addr = Int64.(relative_addr + base) in
            Fact.provide code_region addr size off
          else Fact.return ())

  let sections_of_segments =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(begin
        select (from virtual_program_header)
      end) ~f:ident >>=
    Fact.Seq.iter
      ~f:(fun (_,relative_addr,vsize) ->
          let addr = Int64.(relative_addr + base) in
          Fact.provide section addr vsize)

  let provide ~depends_on ~if_absent x =
    Fact.collect Ogre.Query.(select (from depends_on)) >>= fun s ->
    if Seq.is_empty s then if_absent
    else x

  let code_regions =
    provide code_regions
      ~depends_on:section_entry
      ~if_absent:code_of_segments

  let sections =
    provide sections
      ~depends_on:section_entry
      ~if_absent:sections_of_segments

end

module Relocatable = struct
  module Make(Fact : Ogre.S) = struct
    open Fact.Syntax

    module Base = Base_address(Fact)

    let segments =
      Base.from_sections_offset >>= fun base ->
      Fact.foreach Ogre.Query.(begin
          select (from section_entry $ section_flags)
            ~join:[[field name]]
        end)
        ~f:(fun hdr (_, _, w, x) -> hdr, (w,x)) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun ((name,_,size,off), (w,x)) ->
            let addr = Int64.(base + off) in
            if x then
              Fact.provide segment addr size true w x >>= fun () ->
              Fact.provide mapped addr size off  >>= fun () ->
              Fact.provide named_region addr size name
            else Fact.return ())

    include Relocatable_symbols(Fact)
    include Relocatable_sections(Fact)
    include Code_regions(Fact)
  end
end
