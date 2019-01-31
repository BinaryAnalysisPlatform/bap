open Core_kernel
open Bap.Std

open Bap_llvm_ogre_samples
open Bap_llvm_ogre_types.Scheme
open Image.Scheme
open Bap_llvm_coff_scheme

module Make(Fact : Ogre.S) = struct
  open Fact.Syntax

  let segments =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(
        select (from section_entry
                $ virtual_section_header
                $ section_flags
                $ code_entry)
          ~join:[[field name]])
      ~f:(fun (name, _, size, start) (_,addr,vsize) (_,rwx) _  ->
          name,start,size,addr,vsize,rwx) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, start, size, addr, vsize, (r,w,x)) ->
          let addr = Int64.(base + addr) in
          Fact.provide segment addr vsize r w x >>= fun () ->
          Fact.provide mapped addr size start)

  let sections =
    Fact.require base_address >>= fun base ->
    Fact.foreach Ogre.Query.(
        select (from section_entry $ virtual_section_header)
          ~join:[[field name]])
      ~f:(fun (name,_,_,_) (_,addr,size) -> name,addr,size) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, addr, size) ->
          let addr = Int64.(base + addr) in
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)

  include Symbols(Fact)

end

module Relocatable = struct
  module Make(Fact : Ogre.S) = struct
    open Fact.Syntax

    let segments =
      Fact.require base_address >>= fun base ->
      Fact.foreach Ogre.Query.(
          select (from section_entry
                  $ virtual_section_header
                  $ section_flags
                  $ code_entry)
            ~join:[[field name]])
        ~f:(fun (name, _, size, start) (_,_,vsize) (_,rwx) _  ->
            name,start,size,vsize,rwx) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun (name, start, size, vsize, (r,w,x)) ->
            let addr = Int64.(base + start) in
            Fact.provide segment addr vsize r w x >>= fun () ->
            Fact.provide mapped addr size start)

    let sections =
      Fact.require base_address >>= fun base ->
      Fact.foreach Ogre.Query.(
          select (from section_entry $ virtual_section_header)
            ~join:[[field name]])
        ~f:(fun (name,_,_,off) (_,_,size) -> name,off,size) >>= fun s ->
      Fact.Seq.iter s
        ~f:(fun (name, off, size) ->
            let addr = Int64.(base + off) in
            Fact.provide section addr size >>= fun () ->
            Fact.provide named_region addr size name)

    include Relocatable_symbols(Fact)

  end
end
