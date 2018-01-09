open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Value = Bap_primus_value
module Context = Bap_primus_lisp_context
type context = Context.t

type signature = {
  args : typ list;
  rest : typ option;
  ret  : typ;
}

let symbol_size = 63
let word n = Type n
let var n = Name n
let read_exn s = word (int_of_string (String.strip s))

let read s = Option.try_with (fun () -> read_exn s)
let any = Any

let mem t x = match t with
  | Any | Name _ -> true
  | Symbol -> x = symbol_size
  | Type t -> t = x

let signature ?rest args ret = {
  ret;
  rest;
  args;
}

module Check = struct
  let value typ w =
    mem typ (Word.bitwidth (Value.to_word w))

  let arg typ arg =
    match Var.typ (Arg.lhs arg) with
    | Type.Imm s -> mem typ s
    | _ -> false
end

let pp ppf t = match t with
  | Any | Symbol -> ()
  | Name s -> Format.fprintf ppf "%s" s
  | Type t -> Format.fprintf ppf "%d" t

include Comparable.Make(struct
    type t = typ [@@deriving sexp, compare]
  end)

type t = typ [@@deriving sexp,compare]
