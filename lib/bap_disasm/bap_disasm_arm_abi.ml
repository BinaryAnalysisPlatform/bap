open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi
open Bap_disasm_abi_helpers
open Bap_disasm_arm_env

module Block = Bap_disasm_block
module Symtab = Bap_disasm_symtab
module Insn = Bap_disasm_insn

let registered = ref []

let register abi = registered := abi :: !registered

let create =
  create_abi_getter registered

class gnueabi_basic syms sym = object(self)
  inherit stub
  method! id = ["gnueabi"; "linux"; "unknown"]
  method! specific = false
  method! choose other =
    if List.mem other#id "gnueabi" then
      Int.compare (List.length self#id) (List.length other#id)
    else 0

  method! return_value = Some (Bil.var r0)
  method! args = [r0; r1; r2; r3] |>
                 List.map ~f:(fun r -> None, Bil.var r)
end

let bil_of_block blk =
  Block.insns blk |> List.concat_map ~f:(fun (_,insn) -> Insn.bil insn)

let is_used_before_assigned v bil =
  Bil.is_referenced v bil

let is_assigned_before bound blk var =
  Seq.exists (Block.preds blk) ~f:(fun blk ->
      Block.dfs ~next:Block.preds ~bound blk |>
      Seq.exists ~f:(fun blk ->
          Bil.is_assigned var (bil_of_block blk)))

let is_parameter bound entry var =
  Block.dfs ~next:(Block.succs) ~bound entry |>
  Seq.exists ~f:(fun blk ->
      let bil = bil_of_block blk in
      is_used_before_assigned var bil &&
      not (is_assigned_before bound blk var))

class gnueabi syms fn =
  let bound = unstage (Symtab.create_bound syms fn) in
  let entry = Symtab.entry_of_fn fn in
  let args =
    List.take_while [r0;r1;r2;r3] ~f:(is_parameter bound entry) in
  object(self)
    inherit gnueabi_basic syms fn
    method! args = List.map args ~f:(fun r -> None, Bil.var r)
  end

let () = register (new gnueabi)
