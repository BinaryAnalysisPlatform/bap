open Core_kernel.Std
open Bap.Std

module Context = Primus_context

module type S = sig
  type ('a,'e) m constraint 'e = #Context.t
  val step : ('p,'t) cls -> 't term -> (unit,'e) m
end
