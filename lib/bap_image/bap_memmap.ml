open Core_kernel
open Regular.Std
open Bap_types.Std
open Option.Monad_infix

module Interval = struct
  include Bap_memory
  type point = Addr.t [@@deriving compare, sexp_of]
  let compare x y =
    Addr.compare (min_addr x) (min_addr y)
  let lower = min_addr
  let upper = max_addr
end

include Bap_interval_tree.Make(Interval)

type mem = Bap_memory.t
let min_addr = least
let max_addr = greatest

let pp pp_elt ppf map =
  let module M = Bap_memory in
  let pp_mem ppf mem =
    let a1,a2 = M.min_addr mem, M.max_addr mem in
    Format.fprintf ppf "[%a - %a]" Addr.pp a1 Addr.pp a2 in
  let pp_elt ppf (k,v) =
    Format.fprintf ppf "%a => %a" pp_mem k pp_elt v in
  Seq.pp pp_elt ppf (to_sequence map)

let () = Pretty_printer.register "Bap.Std.Memmap.pp"
