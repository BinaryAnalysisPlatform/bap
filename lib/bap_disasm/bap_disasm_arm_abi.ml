open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi
open Bap_disasm_abi_helpers
open Bap_disasm_arm_env

module Block = Bap_disasm_block
module Insn = Bap_disasm_insn

let registered = ref []

let register abi = registered := abi :: !registered

let create =
  create_abi_getter registered


class gnueabi_basic ?image ?sym mem blk = object(self)
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

module Cfg = Block.Cfg.Imperative

let bil_of_block blk =
  Block.Cfg.Block.insns blk |> List.concat_map ~f:(fun (_,insn) -> Insn.bil insn)

let is_used_before_assigned v bil =
  Bil.is_referenced v bil &&
  Bil.find (object(self) inherit [unit] Bil.finder
    method! visit_move u e goto =
      if compare_var v u = 0
      then goto.return None;
      self#visit_exp e goto
    method! enter_var u goto =
      if compare_var v u = 0
      then goto.return (Some ());
      goto
  end) bil

let is_assigned_before cfg blk var =
  with_return (fun {return} -> Cfg.iter_pred (fun blk ->
      if Bil.is_assigned var (bil_of_block blk)
      then return true) cfg blk;
      false)

let is_parameter cfg var =
  with_return (fun {return} ->
      Cfg.iter_vertex (fun blk ->
          let bil = bil_of_block blk in
          if is_used_before_assigned var bil &&
             not (is_assigned_before cfg blk var)
          then return true) cfg;
      false)

class gnueabi ?image ?sym mem blk =
  let _,cfg = Block.to_imperative_graph ~bound:mem blk in
  let args = List.take_while [r0;r1;r2;r3] ~f:(is_parameter cfg) in
  object(self)
    inherit gnueabi_basic ?image ?sym mem blk
    method! args = List.map args ~f:(fun r -> None, Bil.var r)
  end



let () = register (new gnueabi)
