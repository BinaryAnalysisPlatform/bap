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
        if size = 0L then Fact.return ()
        else
          let full_addr = Int64.(relative_addr + base) in
          Fact.provide named_symbol full_addr name >>= fun () ->
          Fact.provide symbol_chunk full_addr size full_addr >>= fun () ->
          Fact.request code_entry ~that:(fun (n,o,s) ->
              o = off && n = name && s = size) >>= fun f ->
          if f <> None then Fact.provide code_start full_addr
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

module Regions(Fact : Ogre.S) = struct
  open Scheme
  open Fact.Syntax

  let code =
    Fact.foreach Ogre.Query.(begin
        select (from named_region $ code_entry)
          ~join:[[field name];
                 [field size ~from:named_region;
                  field size ~from:code_entry]]
    end)
      ~f:(fun {addr; size;} (_,off,_) -> addr,size,off) >>= fun s ->
    Fact.Seq.iter s ~f:(fun (addr,size,off) ->
        Fact.provide code_region addr size off)

  let data =
    Fact.foreach Ogre.Query.(begin
        select (from named_region $ data_entry)
          ~join:[[field name];
                 [field size ~from:named_region;
                  field size ~from:data_entry]]
    end)
      ~f:(fun {addr; size;} (_,off,_,w) -> addr,size,off,w) >>= fun s ->
      Fact.Seq.iter s ~f:(fun (addr,size,off,w) ->
          Fact.provide data_region addr size off w)

  let regions =
    code >>= fun () ->
    data

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
        if size = 0L then Fact.return ()
        else
          let addr = Int64.(base + off) in
          Fact.provide named_symbol addr name >>= fun () ->
          Fact.provide symbol_chunk addr size addr >>= fun () ->
          Fact.request code_entry ~that:(fun (n,o,s) ->
              o = off && n = name && s = size) >>= fun f ->
          if f <> None then Fact.provide code_start addr
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
