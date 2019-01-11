open Core_kernel
open Bap_lisp__types

module Value = Bap_primus_value
module Context = Bap_lisp__context
type context = Context.t

type signature = {
  args : typ list;
  rest : typ option;
  ret  : typ;
}

let symbol_size = 63
let word n = Type n
let sym = Symbol
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

module Spec = struct
  (* module Type = struct
   *   include Lisp.Program.Type
   *   type t = arch -> Lisp.Type.t
   *   type signature = arch -> Lisp.Type.signature
   *
   *   type parameters = [
   *     | `All of t
   *     | `Gen of t list * t
   *     | `Tuple of t list
   *   ]
   *
   *   module Spec = struct
   *     let any _ = Lisp.Type.any
   *     let var s _ = Lisp.Type.var s
   *     let sym _ = Lisp.Type.sym
   *     let word n _ = Lisp.Type.word n
   *     let int arch =
   *       Lisp.Type.word (Size.in_bits (Arch.addr_size arch))
   *     let bool = word 1
   *     let byte = word 8
   *     let a : t = var "a"
   *     let b : t = var "b"
   *     let c : t = var "c"
   *     let d : t = var "d"
   *
   *     let tuple ts = `Tuple ts
   *     let unit = tuple []
   *     let one t = tuple [t]
   *     let all t = `All t
   *
   *     let (//) : [`Tuple of t list] -> [`All of t] -> parameters =
   *       fun (`Tuple ts) (`All t) -> `Gen (ts,t)
   *
   *     let (@->) (dom : [< parameters]) (cod : t) : signature =
   *       let args,rest = match dom with
   *         | `All t -> [],Some t
   *         | `Tuple ts -> ts,None
   *         | `Gen (ts,t) -> ts, Some t in
   *       fun arch ->
   *         let args = List.map args ~f:(fun t -> t arch) in
   *         let cod = cod arch in
   *         let rest = Option.map rest ~f:(fun t -> t arch) in
   *         Lisp.Type.signature args ?rest cod
   *
   *   end
   * end *)

end

let pp ppf t = match t with
  | Any | Symbol -> ()
  | Name s -> Format.fprintf ppf "%s" s
  | Type t -> Format.fprintf ppf "%d" t


include Comparable.Make(struct
    type t = typ [@@deriving sexp, compare]
  end)

type t = typ [@@deriving sexp,compare]
