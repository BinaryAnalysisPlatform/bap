open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Monads.Std
include Bap_disasm_target_intf

module Res = Monad.Result.Error

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

type bass = addr -> word -> bil -> bil Or_error.t
type bass_category = string

let basses : bass list String.Table.t = String.Table.create ()
let memo = "memo"
let bass = "bil analysis"
let register_bass cat bass = Hashtbl.add_multi basses cat bass

let find_basses cat =
  List.rev @@ Option.value ~default:[] (Hashtbl.find basses cat)

let apply_memo addr code bil =
  Res.List.find_map (find_basses memo)
    ~f:(fun f -> match f addr code bil with
        | Error _ as e -> e
        | Ok bil -> Ok (Some bil)) |> function
  | Ok (Some bil) -> Ok bil
  | Error _ as e -> e
  | _ -> Error (Error.of_string "insn is not memoized")

let apply_analysis addr code bil =
  let open Res in
  let rec apply bil = function
    | [] -> bil
    | f :: fs ->
      bil >>= fun bil -> apply (f addr code bil) fs in
  apply (type_check bil) (find_basses bass)

let get_opcode mem =
  let open Res in
  let len = Memory.length mem in
  let rec run n code addr =
    if n = len then return code
    else
      Memory.get ~addr mem >>= fun word ->
      run (n + 1) (Word.concat code word) (Word.succ addr) in
  let addr = Memory.min_addr mem in
  Memory.get ~addr mem >>= fun fst ->
  run 1 fst (Addr.succ addr)

module Make(T : Target) = struct
  include T

  let lift memory insn =
    let addr = Memory.min_addr memory in
    match get_opcode memory with
    | Error _ as e -> e
    | Ok code ->
      match apply_memo addr code [] with
      | Ok bil -> Ok bil
      | _ ->
        Or_error.(T.lift memory insn >>= apply_analysis addr code)
end

let register_target arch (module Target : Target) =
  let module T = Make(Target) in
  Hashtbl.set targets ~key:arch ~data:(module T)
