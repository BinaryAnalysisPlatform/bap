open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_std
open Bap_ir
module Helpers = Bap_disasm_abi_helpers

module Ssa = Bap_sema_ssa
open Bap_sema_free_vars

let compute_args sub ins =
  let free = free_vars_of_sub sub in
  List.filter ins ~f:(Set.mem free) |>
  List.mapi ~f:(fun i var ->
      let name = sprintf "arg_%d" i in
      Var.create name (Var.typ var), Bil.var var)


class input_register_only sub name namespace ins =
  let args = compute_args sub ins in
  object(self)
    inherit Helpers.stub
    method! id = name :: namespace
    method! specific = false
    method! choose other =
      if List.mem other#id name then
        Int.compare (List.length self#id) (List.length other#id)
      else 0
    method! return_value = None
    method! args = args
  end

class gnueabi_registers_only sub = object(self)
  inherit input_register_only sub
      "gnueabi" ["linux"; "unknown"]
      ARM.CPU.([r0;r1;r2;r3])
end

let register () = ARM.ABI.register (new gnueabi_registers_only)

let infer_args sub arch =
  let module Target = (val target_of_arch arch) in
  let abi = Target.ABI.create sub in
  let add sub (var,exp) =
    Term.append arg_t sub @@ Ir_arg.create var exp in
  let sub = List.fold abi#args ~init:sub ~f:add in
  Option.value_map abi#return_value ~default:sub ~f:(add sub)
