open Core_kernel.Std
open Sequence

type 'a seq = 'a t

let of_array arr =
  init (Array.length arr) ~f:Array.(unsafe_get arr)

let sexp_of_seq sexp_of_elt t =
  <:sexp_of<list>> sexp_of_elt (to_list t)

let seq_of_sexp elt_of_sexp t =
  of_list (<:of_sexp<list>> elt_of_sexp t)


let cons x xs = append (singleton x) xs

module Export = struct
  let (^::) = cons
end

open Export
