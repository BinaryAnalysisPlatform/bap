open Core_kernel
open Format

module type S  = Bap_knowledge_domain_type.S
module Order = Bap_knowledge_domain_type.Order


module Chain = struct
  module type T = Bap_knowledge_domain_type.Chain
  module Make(T : T) : S with type t = T.t = struct
    include T
    let partial x y : Order.partial = match compare x y with
      | 1 -> GE
      | 0 -> EQ
      | _ -> LE
  end
end

module Map = struct
  module type Eq = Bap_knowledge_domain_type.Eq

  module Make(K : Comparator.S)(V : Eq) = struct
    type t = V.t Map.M(K).t

    let empty = Map.empty (module K)

    let inspect xs =
      Sexp.List (Map.keys xs |> List.map ~f:K.comparator.sexp_of_t)

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
  include Base.Comparable.S with type t := t
end = struct
  let empty = Int63.zero
  let inspect = Int63.sexp_of_t
  let partial x y : Order.partial = match compare x y with
    | 0 -> EQ
    | 1 -> GE
    | _ -> LE
  include Int63
end

module Label : S with type t = Bap_knowledge_label.t = Chain.Make(struct
    include Bap_knowledge_label
    let empty = root
    let inspect lbl = Sexp.Atom (asprintf "%a" Bap_knowledge_label.pp lbl)
  end)
