open Bap.Std
open Bap_api

type 'a pass = attr -> 'a term -> 'a term

module Arg : sig
  val register : (pos -> arg pass) -> unit
  val apply : pos -> arg pass
end


module Sub : sig
  val register : sub pass -> unit
  val apply : sub pass
end
