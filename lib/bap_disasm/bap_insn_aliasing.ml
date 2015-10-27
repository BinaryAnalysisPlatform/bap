open Core_kernel.Std
open Bap_types.Std

module Dis = Bap_disasm_basic
module Memory = Bap_memory

let fold_consts = Bil.(fixpoint fold_consts)

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

let rec dests_of_bil bil =
  fold_consts bil |> List.concat_map ~f:dests_of_stmt
and dests_of_stmt = let open Bil in function
  | Jmp (Bil.Int addr) -> [Some addr,`Jump]
  | Jmp (_) -> [None, `Jump]
  | If (_,yes,no) -> merge_branches yes no
  | While (_,ss) -> dests_of_bil ss
  | _ -> []
and merge_branches yes no =
  let x = dests_of_bil yes and y = dests_of_bil no in
  let kind = kind_of_branches x y in
  List.(rev_append x y >>| fun (a,_) -> a,kind)

let is_exec_ok gmem_min gmem_max lmem  =
  Addr.((Memory.min_addr lmem) >= gmem_min) &&
  Addr.((Memory.max_addr lmem) <= gmem_max)

let targ_in_mem gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr < gmem_max)
