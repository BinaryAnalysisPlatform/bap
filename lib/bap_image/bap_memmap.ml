open Bap_core_theory
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


let sexp_of_addr x =
  Sexp.Atom (Format.asprintf "%a" Addr.pp x)

let sexp_of_region sexp_of_value (mem,v) = Sexp.List [
    List [
      Atom "lower";
      sexp_of_addr (Bap_memory.min_addr mem)
    ];
    List [
      Atom "upper";
      sexp_of_addr (Bap_memory.max_addr mem)
    ];
    List [
      Atom "value";
      sexp_of_value v;
    ]
  ]


let sexp_of_t sexp_of_value map =
  let sexp_of_element = sexp_of_region sexp_of_value in
  [%sexp_of: element Seq.t] (to_sequence map)


let inspect = sexp_of_t @@ fun v -> Sexp.List [
    Atom (Value.tagname v);
    Atom (Value.to_string v);
  ]

let compare x y =
  Seq.compare (fun (m1,_) (m2,_) ->
      Interval.compare m1 m2)
    (to_sequence x)
    (to_sequence y)

let domain = KB.Domain.flat "memmap"
    ~empty
    ~inspect
    ~equal:(fun x y -> compare x y = 0)

let () = Pretty_printer.register "Bap.Std.Memmap.pp"
