module Iterator = Bap_primus_iterator

module MCG : sig
  module type S = sig
    include Iterator.Infinite.S with type dom = Bitvec.t
    val size : int
    val create : int -> t
  end

  val create : ?min:Bitvec.t -> ?max:Bitvec.t -> int -> (module S)
  val create_small : ?min:int -> ?max:int -> int -> (module S)

  module M8  : S
  module M16 : S
  module M32 : S
  module M64 : S
end
