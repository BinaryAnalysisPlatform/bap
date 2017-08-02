open Core_kernel.Std
open Bap.Std

open Bap_primus_types

let sexp_of_tid t = Sexp.Atom (Tid.name t)
let sexp_of_var v = Sexp.Atom (Var.name v)

let string_of_byte w = sprintf "%02x" @@ ok_exn (Word.to_int w)
let string_of_word w = Word.string_of_value w
let string_of_value {value=w} = string_of_word w
let sexp_of_word w = Sexp.Atom (string_of_word w)
let sexp_of_value {value} = sexp_of_word value
let sexp_of_byte w = Sexp.Atom (string_of_byte w)
let sexp_of_binding (v,r) = [%sexp ((v : var), (r : value))]
let sexp_of_values (x,y) = [%sexp ((x:value),(y:value))]
let sexp_of_move (a,w) = [%sexp ((a : word), (w : word))]
let sexps atoms = Sexp.List (List.map ~f:(fun x -> Sexp.Atom x) atoms)
