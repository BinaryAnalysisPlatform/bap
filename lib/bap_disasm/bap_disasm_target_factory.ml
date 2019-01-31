open Core_kernel
open Bap_types.Std
open Bap_image_std
include Bap_disasm_target_intf

let create_stub_target arch =
  let module Lifter = struct
    let lift _ _ = Or_error.error_string "not implemented"
    let addr_size = Arch.addr_size arch

    module CPU = struct
      let gpr = Var.Set.empty
      let nil = Var.create "nil" (Type.imm (Size.in_bits addr_size))
      let mem = Var.create "mem" (Type.mem addr_size `r8)
      let pc = nil
      let sp = nil
      let sp = nil
      let zf = nil
      let cf = nil
      let vf = nil
      let nf = nil
      let addr_of_pc = Memory.max_addr
      let no _ = false
      let is_reg = no
      let is_flag = no
      let is_sp = no
      let is_bp = no
      let is_pc = no
      let is_zf = no
      let is_cf = no
      let is_vf = no
      let is_nf = no
      let is_mem = no
    end
  end in
  (module Lifter : Target)

let targets = Arch.Table.create ()

let target_of_arch =
  let get arch = match Hashtbl.find targets arch with
    | Some target -> target
    | None ->
      let target = create_stub_target arch in
      Hashtbl.set targets ~key:arch ~data:target;
      target in
  get

let type_check bil = match Type.check bil with
  | Error te ->
    let err  =
      Error.createf "The lifted code is not well-typed: %s"
        (Type.Error.to_string te) in
    Error err
  | Ok () -> Ok bil

let apply_passes bil =
  let f bil =
    List.fold ~init:bil (Bil.selected_passes ()) ~f:(fun bil f -> f bil) in
  Or_error.(type_check bil >>| Bil.fixpoint f >>= type_check)

module Make(T : Target) = struct
  include T

  let lift mem insn = match T.lift mem insn with
    | Error _ as e -> e
    | Ok bil -> apply_passes bil
end

let register_target arch (module Target : Target) =
  let module T = Make(Target) in
  Hashtbl.set targets ~key:arch ~data:(module T)
