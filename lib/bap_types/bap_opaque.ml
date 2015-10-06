open Core_kernel.Std
open Bap_regular

module type S = sig
  type t
  include Comparable with type t := t
  include Hashable with type t := t
end

module Make(M : sig
    type t with compare
    val hash : t -> int
  end) = struct
  module M = struct
    include M
    let sexp_of_t = sexp_of_opaque
    let t_of_sexp = opaque_of_sexp
  end
  include Comparable.Make(M)
  include Hashable.Make(M)
end
