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

type memo = addr -> word -> bil option
type bass = addr -> word -> bil -> bil Or_error.t

let memo = ref (fun _ _ -> None)
let basses : bass String.Table.t = String.Table.create ()

let register_memo memo' = memo := memo'
let register_bass name bass = Hashtbl.update basses name (fun _ -> bass)
let bass_list () = Hashtbl.to_alist basses |> List.map ~f:snd

let apply_analysis addr code bil =
  let open Or_error in
  let bs = bass_list () in
  List.fold bs ~init:(type_check bil)
    ~f:(fun bil f -> bil >>= f addr code) >>= type_check

let get_opcode memory =
  let open Or_error in
  let len = Memory.length memory in
  let rec run n code addr =
    if n = len then return code
    else
      Memory.get ~addr memory >>= fun word ->
      run (n + 1) (Word.concat code word) (Word.succ addr) in
  let addr = Memory.min_addr memory in
  Memory.get ~addr memory >>= fun fst ->
  run 1 fst (Addr.succ addr)

module Make(T : Target) = struct
  include T

  let lift memory insn =
    let addr = Memory.min_addr memory in
    match get_opcode memory with
    | Error _ as e -> e
    | Ok code ->
      match !memo addr code with
      | Some bil -> Ok bil
      | None ->
        Or_error.(T.lift memory insn >>= apply_analysis addr code)
end

let register_target arch (module Target : Target) =
  let module T = Make(Target) in
  Hashtbl.set targets ~key:arch ~data:(module T)
