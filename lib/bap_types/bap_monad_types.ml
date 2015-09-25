open Core_kernel.Std



module type State = sig
  type (+'a,'s) t

  include Monad.S2 with type ('a,'s) t := ('a,'s) t

  val put : 's -> (unit,'s) t
  val get : unit -> ('s,'s) t
  val gets : ('s -> 'r) -> ('r,'s) t
  val modify : ('a,'s) t -> ('s -> 's) -> ('a,'s) t


  val run : ('a,'s) t -> 's -> 'a * 's
  val eval : ('a,'s) t -> 's -> 'a
  val exec : ('a,'s) t -> 's -> 's
end
