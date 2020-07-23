open Core_kernel
open Bap.Std

open Bap_llvm_ogre_types

module Symbols(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax

  let symbols =
    Fact.require base_address >>= fun base ->
    Fact.collect Ogre.Query.(select (from symbol_entry)) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, relative_addr, size, off) ->
        if Int64.(size = 0L) then Fact.return ()
        else
          let full_addr = Int64.(relative_addr + base) in
          Fact.provide named_symbol full_addr name >>= fun () ->
          Fact.provide symbol_chunk full_addr size full_addr >>= fun () ->
          Fact.request code_entry ~that:(fun (n,o,s) ->
              Int64.(o = off) && String.(n = name) && Int64.(s = size)) >>= fun f ->
          if Option.is_some f then Fact.provide code_start full_addr
          else Fact.return ())
end

module Sections(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax

  let sections =
    Fact.require base_address >>= fun base ->
    Fact.collect Ogre.Query.(select (from section_entry)) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, relative_addr, size, _) ->
          let addr = Int64.(relative_addr + base) in
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)
end

module Relocatable_symbols(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax
  let relocations =
    Fact.require base_address >>= fun base ->
    Fact.collect
      Ogre.Query.(select (from ref_internal)) >>= fun ints ->
    Fact.Seq.iter ints ~f:(fun (sym_off, rel_off) ->
        let symbol_addr = Int64.(sym_off + base) in
        let relocation_addr = Int64.(rel_off + base) in
        Fact.provide relocation relocation_addr symbol_addr)

  let externals =
    Fact.require base_address >>= fun base ->
    Fact.collect
      Ogre.Query.(select (from ref_external)) >>= fun exts ->
    Fact.Seq.iter exts ~f:(fun (off, name) ->
        Fact.provide external_reference Int64.(base + off) name)

  let symbols =
    relocations >>= fun () ->
    externals >>= fun () ->
    Fact.require base_address >>= fun base ->
    Fact.collect Ogre.Query.(select (from symbol_entry)) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (name, _, size, off) ->
        if Int64.(size = 0L) then Fact.return ()
        else
          let addr = Int64.(base + off) in
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request code_entry ~that:(fun (n,o,s) ->
              Int64.(o = off) && String.(n = name) && Int64.(s = size)) >>= fun f ->
          if Option.is_some f then Fact.provide code_start addr
          else Fact.return ())

end

module Relocatable_sections(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax


  let sections =
    Fact.require base_address >>= fun base ->
    Fact.collect Ogre.Query.(select (from section_entry)) >>= fun s ->
    Fact.Seq.iter s
      ~f:(fun (name, _, size, off) ->
          let addr = Int64.(base + off) in
          Fact.provide section addr size >>= fun () ->
          Fact.provide named_region addr size name)
end


module Code_regions(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax

  let code_regions =
    Fact.foreach Ogre.Query.(begin
        select (from named_region $ code_entry)
          ~join:[[field name];
                 [field size ~from:named_region;
                  field size ~from:code_entry]]
      end)
      ~f:(fun {addr; size;} (_,off,_) -> addr,size,off) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (addr,size,off) ->
        Fact.provide code_region addr size off)

end
