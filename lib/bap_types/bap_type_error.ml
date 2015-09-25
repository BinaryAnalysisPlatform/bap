open Core_kernel.Std
open Bap_common

type type_error = [
  | `bad_kind of [`mem | `imm]
  | `bad_type of typ * typ
  | `bad_cast
] with bin_io, compare, sexp

type t = type_error with bin_io, compare, sexp

let bad_mem  = (`bad_kind `mem)
let bad_imm  = (`bad_kind `imm)
let bad_cast = `bad_cast
let bad_type ~exp ~got = (`bad_type (exp,got))

let to_string : type_error -> string = function
  | `bad_kind `mem -> "expected storage, got immediate value"
  | `bad_kind `imm -> "expected immediate value, got storage"
  | `bad_cast -> "malformed cast arguments"
  | `bad_type (t1,t2) ->
    sprintf "expected value of type %a, but got %a"
      Bap_type.pps t1 Bap_type.pps t2

include Regular.Make(struct
    type t = type_error with bin_io, compare, sexp

    let pp ppf e =
      Format.fprintf ppf "%s" (to_string e)

    let hash = Hashtbl.hash
    let module_name = Some "Bap.Std.Type_error"
  end)
