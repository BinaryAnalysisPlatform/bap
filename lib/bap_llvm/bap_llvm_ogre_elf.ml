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
end

module Relocatable = struct
  module Make(Fact : Ogre.S) = struct
    open Fact.Syntax

    let segments =
      Fact.require base_address >>= fun base ->
      Fact.foreach Ogre.Query.(begin
          select (from section_entry $ section_flags)
            ~join:[[field name]]
        end)
        ~f:(fun hdr (_, w, x) -> hdr, (w,x)) >>= fun s ->
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
  end
end
