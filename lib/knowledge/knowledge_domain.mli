open Core_kernel

module type S = Knowledge_domain_type.S

module Order = Knowledge_domain_type.Order

module Chain : sig
  module type T = Knowledge_domain_type.Chain
  module Make(Chain : T) : S with type t = Chain.t
end

module Map : sig
  module type Eq = Knowledge_domain_type.Eq

  module Make(K : Comparable.S)(V : Eq) : S with type t = V.t K.Map.t
end

module Counter : sig
  type t
  val zero : t
  val succ : t -> t
  val pp : Format.formatter -> t -> unit
  include S with type t := t
  include Comparable.S with type t := t
end

module Label : S with type t = Knowledge_label.t
