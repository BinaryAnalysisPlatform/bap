open Core_kernel
open Caml.Format

type data = Data_Effect
type ctrl = Ctrl_Effect
type 'a t =
  | Data : data t
  | Ctrl : ctrl t
  | Unit : unit t

let compare : type s. s t -> s t -> int = fun _ _ -> 0

let type_equal : type a b.
  a t -> b t -> (a t, b t) Type_equal.t option = fun x y ->
  match x,y with
  | Data,Data -> Some Type_equal.T
  | Ctrl,Ctrl -> Some Type_equal.T
  | Unit,Unit -> Some Type_equal.T
  | (Data,_) -> None
  | (Ctrl,_) -> None
  | (Unit,_) -> None

let same x y = Option.is_some (type_equal x y)

let pp (type s) ppf (x : s t) = match x with
  | Data -> fprintf ppf "data eff"
  | Ctrl -> fprintf ppf "ctrl eff"
  | Unit -> fprintf ppf "unit eff"

let name x = asprintf "%a" pp x

let data = Data
let ctrl = Ctrl
let unit = Unit
