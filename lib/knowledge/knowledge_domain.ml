open Core_kernel
open Monads.Std
open Format

module type S  = Knowledge_domain_type.S
module Order = Knowledge_domain_type.Order

module Chain = struct
  module type T = Knowledge_domain_type.Chain
  module Make(T : T) : S with type t = T.t = struct
    include T
    let partial x y : Order.partial = match compare x y with
      | 1 -> GE
      | 0 -> EQ
      | _ -> LE
  end
end

module Map = struct
  module type Eq = Knowledge_domain_type.Eq

  module Make(K : Comparable.S)(V : Eq) = struct
    type t = V.t K.Map.t

    let empty = K.Map.empty

    let inspect xs =
      Sexp.List (Map.keys xs |> List.map ~f:(K.Map.Key.sexp_of_t))

    let partial x y =
      Map.symmetric_diff x y ~data_equal:V.equal |>
      Sequence.fold ~init:(0,0,0) ~f:(fun (l,m,r) -> function
          | (_,`Left _)     -> (l+1,m,r)
          | (_,`Right _)    -> (l,m,r+1)
          | (_, `Unequal _) -> (l,m+1,r)) |> function
      | 0,0,0 -> Order.EQ
      | 0,0,_ -> Order.LE
      | _,0,0 -> Order.GE
      | _,_,_ -> Order.NC
  end
end
module Counter : sig
  type t
  val zero : t
  val succ : t -> t
  val pp : Format.formatter -> t -> unit
  include S with type t := t
  include Comparable.S with type t := t
end = struct
  let empty = Int63.zero
  let inspect = Int63.sexp_of_t
  let partial x y : Order.partial = match compare x y with
    | 0 -> EQ
    | 1 -> GE
    | _ -> LE
  include Int63
end

module Label : S with type t = Knowledge_label.t = Chain.Make(struct
    include Knowledge_label
    let empty = root
    let inspect lbl = Sexp.Atom (asprintf "%a" Knowledge_label.pp lbl)
  end)
