open Core_kernel
open Bap_core_theory
open Bap_lisp__types
module Context = Bap_lisp__context
type context = Context.t

type signature = {
  args : typ list;
  rest : typ option;
  ret  : typ;
}

let symbol_size = 63
let sort s = Type (Theory.Value.Sort.forget s)
let bool = sort Theory.Bool.t
let word n = sort (Theory.Bitv.define n)
let sym = Symbol
let var n = Name n

type read_error = Empty | Not_sexp | Bad_sort

(* let rec parse_sort : Sexp.t -> (_,read_error) result = function
 *   | Atom "Bool" -> Ok Bool
 *   | Atom s -> Ok (Cons (s,[]))
 *   | List (Atom name :: ps) ->
 *     Result.map (parse_params ps) ~f:(fun ps -> Sort.Cons (name,ps))
 *   | List _ -> Error Bad_sort
 * and parse_params ps = Result.all (List.map ps ~f:parse_param)
 * and parse_param = function
 *   | Atom x when Char.is_digit x.[0] -> Ok (Index (int_of_string x))
 *   | x -> Result.map (parse_sort x) ~f:(fun x -> Sort.Sort x) *)


let read s =
  if String.length s < 1 then Error Empty
  else
  if Char.is_lowercase s.[0]
  then Ok (Name s)
  else match Sexp.of_string s with
    | exception _ -> Error Not_sexp
    | s ->
      failwith "Sort parsing is not implemented yet"
(* Result.map (parse_sort s) ~f:(fun s -> Type s) *)

let any = Any


let signature ?rest args ret = {
  ret;
  rest;
  args;
}

module Check = struct
  let sort typ s = match typ with
    | Any | Name _ -> true
    | Symbol -> false
    | Type s' ->
      let s = Theory.Value.Sort.forget s in
      Theory.Value.Sort.Top.compare s s' = 0
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
  | Type _ -> ()


include Comparable.Make(struct
    type t = typ [@@deriving sexp, compare]
  end)

type t = typ [@@deriving sexp,compare]
