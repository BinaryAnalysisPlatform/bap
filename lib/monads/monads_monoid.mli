open Core_kernel.Std

module type Base = sig
  type t
  val zero : t
  val plus : t -> t -> t
end


module type S = sig
  include Base
  val concat : t list -> t
  val (@@) : t -> t -> t
end

module Make(M : Base) : S with type t := M.t

module TList(T : T) : sig type t = T.t list end

module Stack : sig
  module Make (T : T) : S with type t := TList(T).t
end

module List : sig
  module Make (T : T) : S with type t := TList(T).t
end

module String : S with type t = string

module Set : sig
  module Make(S : Set.S) : S with type t := S.t
end

module Int : sig
  include S with type t = int
  module Sum : S with type t = int
  module Product : S with type t = int
end

module Float : sig
  include S with type t = float
  module Sum : S with type t = float
  module Product : S with type t = float
end
