open Core_kernel
open Regular.Std
open Bap_common

type t = [
  | `bad_kind of [`mem | `imm]
  | `bad_type of typ * typ
  | `bad_cast
] [@@deriving bin_io, compare, sexp]

type type_error = t [@@deriving bin_io, compare, sexp]

exception T of t [@@deriving sexp_of]

let bad_mem  = `bad_kind `mem
let bad_imm  = `bad_kind `imm
let bad_cast = `bad_cast
let bad_type ~exp ~got = `bad_type (exp,got)

let expect_mem () = raise (T (`bad_kind `mem))
let expect_imm () = raise (T (`bad_kind `imm))
let wrong_cast () = raise (T (`bad_cast))
let expect e ~got = raise (T (`bad_type (e,got)))

let to_string : type_error -> string = function
  | `bad_kind `mem -> "expected storage, got immediate value"
  | `bad_kind `imm -> "expected immediate value, got storage"
  | `bad_cast -> "malformed cast arguments"
  | `bad_type (t1,t2) ->
    sprintf "expected value of type %a, but got %a"
      Bap_type.pps t1 Bap_type.pps t2

include Regular.Make(struct
    type t = type_error [@@deriving bin_io, compare, sexp]

    let pp ppf e =
      Format.fprintf ppf "%s" (to_string e)

    let hash = Hashtbl.hash
    let module_name = Some "Bap.Std.Type_error"
    let version = "1.0.0"
  end)
