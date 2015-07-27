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

let is_empty : 'a seq -> bool = length_is_bounded_by ~max:0


let filter s ~f = filteri s ~f:(fun _ x -> f x)

(* honestly stolen from newer core_kernel, to
   get compatiblity with older library versions *)
let compare compare_a t1 t2 =
  with_return (fun r ->
      iter (zip_full t1 t2) ~f:(function
          | `Left _        -> r.return 1
          | `Right _       -> r.return (-1)
          | `Both (v1, v2) ->
            let c = compare_a v1 v2 in
            if c <> 0
            then r.return c);
      0);


module Export = struct
  let (^::) = cons
end

open Export
