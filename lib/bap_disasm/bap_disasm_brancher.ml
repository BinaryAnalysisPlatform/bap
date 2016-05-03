open Core_kernel.Std
open Bap_types.Std
open Bap_image_std


module Source = Bap_disasm_source
module Targets = Bap_disasm_target_factory
module Dis = Bap_disasm_basic

type edge = Bap_disasm_block.edge [@@deriving sexp]
type dest = addr option * edge [@@deriving sexp]
type dests = dest list [@@deriving sexp]
type full_insn = Bap_disasm_basic.full_insn

type t = Brancher of (mem -> full_insn -> dests)
type brancher = t

let create f = Brancher f
let resolve (Brancher f) = f

let kind_of_dests = function
  | xs when List.for_all xs ~f:(fun (_,x) -> x = `Fall) -> `Fall
  | xs -> if List.exists  xs ~f:(fun (_,x) -> x = `Jump)
    then `Jump
    else `Cond

let kind_of_branches t f =
  match kind_of_dests t, kind_of_dests f with
  | `Jump,`Jump -> `Jump
  | `Fall,`Fall -> `Fall
  | _           -> `Cond

let fold_consts = Bil.(fixpoint fold_consts)

let rec dests_of_bil bil : dests =
  fold_consts bil |> List.concat_map ~f:dests_of_stmt
and dests_of_stmt = function
  | Bil.Jmp (Bil.Int addr) -> [Some addr,`Jump]
  | Bil.Jmp (_) -> [None, `Jump]
  | Bil.If (_,yes,no) -> merge_branches yes no
  | Bil.While (_,ss) -> dests_of_bil ss
  | _ -> []
and merge_branches yes no =
  let x = dests_of_bil yes and y = dests_of_bil no in
  let kind = kind_of_branches x y in
  List.(rev_append x y >>| fun (a,_) -> a,kind)

let dests_of_bil arch =
  let module Target = (val Targets.target_of_arch arch) in
  fun mem insn ->
    let next = Addr.succ (Memory.max_addr mem) in
    let dests = match Target.lift mem insn with
      | Error _ -> []
      | Ok bil -> dests_of_bil bil in
    let is = Dis.Insn.is insn in
    let fall = Some next, `Fall in
    match kind_of_dests dests with
    | `Fall when is `Return -> []
    | `Jump when is `Call -> fall :: dests
    | `Cond | `Fall -> fall :: dests
    | _ -> dests


let of_bil arch = create (dests_of_bil arch)


module Factory = Source.Factory(struct type nonrec t = t end)
