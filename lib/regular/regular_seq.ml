open Core_kernel
open Sequence


let of_array arr =
  init (Array.length arr) ~f:Array.(unsafe_get arr)

let sexp_of_seq sexp_of_elt t =
  [%sexp_of:list] sexp_of_elt (to_list t)

let seq_of_sexp elt_of_sexp t =
  of_list ([%of_sexp:list] elt_of_sexp t)

let t_of_sexp = seq_of_sexp
let sexp_of_t = sexp_of_seq


let cons x xs = append (singleton x) xs

let is_empty : 'a Sequence.t -> bool = length_is_bounded_by ~max:0


let filter s ~f = filteri s ~f:(fun _ x -> f x)

(* honestly stolen from newer core_kernel, to
   get compatibility with older library versions *)
let compare compare_a t1 t2 =
  with_return (fun r ->
      iter (zip_full t1 t2) ~f:(function
          | `Left _        -> r.return 1
          | `Right _       -> r.return (-1)
          | `Both (v1, v2) ->
            let c = compare_a v1 v2 in
            if c <> 0
            then r.return c);
      0)

module Binable = Bin_prot.Utils.Make_binable1(struct
    module Binable = List
    type 'a t = 'a Sequence.t
    let to_binable = Sequence.to_list
    let of_binable = Sequence.of_list
  end)[@@warning "-D"]

include Binable
let compare_seq = compare

module Export = struct
  let (^::) = cons
end

open Format
let max_printer_depth = ref 100

let pp_comma ppf () = pp_print_string ppf ", "

let pp_body ?max pp_elem ppf seq =
  mapi seq ~f:(fun i elem () -> match max with
      | Some m when m = i -> fprintf ppf "..."
      | _ -> pp_elem ppf elem) |>
  intersperse ~sep:(pp_comma ppf) |>
  iter ~f:(fun pp -> pp ())

let pp_head ppf = fprintf ppf "{@[<2>"
let pp_tail ppf = fprintf ppf "}@]"

let pp_all pp_elem ppf seq =
  pp_head ppf;
  pp_body pp_elem ppf seq;
  pp_tail ppf


let pp_some pp_elt ppf xs =
  let xs = take xs (!max_printer_depth + 1) in
  pp_head ppf;
  pp_body ~max:!max_printer_depth pp_elt ppf xs;
  pp_tail ppf

let pp pp_elt ppf xs =
  if Sys.interactive.contents
  then pp_some pp_elt ppf xs
  else pp_all  pp_elt ppf xs

type 'a seq = 'a t [@@deriving bin_io, compare, sexp]

let () = Pretty_printer.register "Regular.Std.Seq.pp"
