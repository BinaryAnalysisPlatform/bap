open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
open Bap_future.Std
include Self ()

open Bap_main
open KB.Syntax

let relink prog links =
  (object
    inherit Term.mapper

    method! map_jmp jmp =
      match Jmp.alt jmp with
      | None -> jmp
      | Some alt -> match Jmp.resolve alt with
        | Second _ -> jmp
        | First tid -> match Map.find links tid with
          | Some tid' ->
            Jmp.with_alt jmp (Some (Jmp.resolved tid'))
          | _ -> jmp
  end)#run prog

module Plt = struct

  let is_section name v =
    match Value.get Image.section v with
    | Some x -> String.(x = name)
    | _ -> false

  let section_memory mem sec_name =
    let collect_addresses addrs (mem,_ )=
      Memory.foldi mem ~word_size:`r8 ~init:addrs
        ~f:(fun addr _ acc -> Set.add acc (Word.to_bitvec addr)) in
    Memmap.filter mem ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.fold ~init:(Set.empty (module Bitvec_order)) ~f:collect_addresses

  let provide mem =
    let stubs = section_memory mem ".plt" in
    KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
    KB.collect Theory.Label.addr label >>| function
    | Some addr when Set.mem stubs addr -> Some ()
    | _ ->    None
end

let update prog = relink prog (Stub_resolver.run prog)

let main proj =
  Project.with_program proj (update @@ Project.program proj)

let () = Extension.documentation {|
  # DESCRIPTION

  Provides an abi pass that transforms a program by substituting calls
  to stubs with calls to real subroutines when they are present in
  the binary.

|}

let () = Extension.declare @@ fun _ctxt ->
  Stream.observe Project.Info.data @@ Plt.provide;
  Bap_abi.register_pass main;
  Ok ()
