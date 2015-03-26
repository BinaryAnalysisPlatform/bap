open Core_kernel.Std
open Bap.Std
open Format

type t = {
  precision : float;
  recall : float;
  fp     : int;
  tp     : int;
  fn     : int;
  f_05     : float;
  tool   : string
}

val compare : addr list -> addr list -> string -> t

val pp : t list -> unit
