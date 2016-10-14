open Bap_types.Std
open Regular.Std
open Bap_ir
open Bap_expi_types
open Bap_monad_types

type t = tid

type set = Tid.Set.t [@@deriving bin_io, compare, sexp]
type map = set Var.Map.t [@@deriving bin_io, compare, sexp]

val reg : t tag
val ptr : t tag

val regs : map tag

val ptrs : map tag

val merge : map -> map -> map

class context :  object('s)
  method taint_reg : Bil.result -> set -> 's
  method taint_ptr : addr -> size -> set -> 's
  method reg_taints : Bil.result -> set
  method ptr_taints : addr -> set
  method all_taints : set
end

module type S = sig
  type ('a,'e) state
  module Expi : Expi.S with type ('a,'e) state = ('a,'e) state
  class ['a] propagator : object('s)
    constraint 'a = #context
    inherit ['a] Expi.t
  end
end

module Make(M : State) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Bap_monad.State.t


val pp_set : Format.formatter -> set -> unit

val pp_map : Format.formatter -> map -> unit

module Map : Regular.S with type t = map
