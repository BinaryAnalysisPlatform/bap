open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
include Bap_disasm_target_intf


let create_stub_target () =
  let module Lifter = struct
    let lift _ _ = Or_error.error_string "not implemented"

    module CPU = struct
      let gpr = Var.Set.empty
      let nil = Var.create "nil" reg8_t
      let mem = nil
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
      let target = create_stub_target () in
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

(* bass = bil analysis  *)
type bass = bil -> bil Or_error.t

let basses = String.Table.create ()
let selected = Queue.create ()

let register_bass name (bass:bass) =
  Hashtbl.update basses name (fun _ -> bass)

let find_bass name = Hashtbl.find basses name
let run_bass bass = Queue.enqueue selected bass
let bass_list () = Hashtbl.keys basses

let apply_basses bil =
  let open Or_error in
  let rec apply bil = function
    | [] -> bil
    | f :: fs ->
      bil >>= fun bil -> apply (f bil) fs in
  apply (type_check bil) (Queue.to_list selected)

module Make(T : Target) = struct
  include T

  let lift memory insn =
    Or_error.(T.lift memory insn >>= apply_basses)
end

let register_target arch (module Target : Target) =
  let module T = Make(Target) in
  Hashtbl.set targets ~key:arch ~data:(module T)
