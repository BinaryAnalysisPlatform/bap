open Core_kernel.Std

module Std = struct
  module Tid = Type_equal.Id
  type 'a language = 'a Tid.t
  module Language = struct
    type 'a t = 'a language

    let create ~name = Tid.create ~name sexp_of_opaque

    let name = Tid.name

    module type Property = sig
      type t
      type 'a data

      val create : unit -> t
      val set : t -> 'a language -> 'a data -> unit
      val get : t -> 'a language -> 'a data option
    end


    module Property(T : T1) : Property with type 'a data = 'a T.t = struct
      module U = Univ_map.Make(struct
          type 'a t = 'a T.t
          let sexp_of_t  _ = sexp_of_opaque
        end)
      type t = U.t ref
      type 'a data = 'a T.t

      let empty = U.empty

      let create () = ref empty
      let set t lang data = t := U.set !t lang data
      let get t lang = U.find !t lang
    end
  end
end
