open Bap_knowledge
open Format

module type Element = sig
  type t
  val pp : formatter -> t -> unit
end

module Category : Element
module Name     : Element
module Descr    : Element
type index = (Category.t * (Name.t * Descr.t) list) list

val generate_index : Bap_lisp__program.t -> index
