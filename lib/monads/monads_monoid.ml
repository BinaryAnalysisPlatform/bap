open Core_kernel

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
module Make(M : Base) : S with type t := M.t = struct
  include M
  let concat = List.fold ~init:zero ~f:plus
  let (@@) = plus
end

module Unit : S with type t = unit = struct
  type t = unit
  let zero = ()
  let plus _ _ = ()
  let concat _ = ()
  let (@@) = plus
end

module TList(T : T) = struct type t = T.t list end
module Stack = struct
  module Make(T : T) : S with type t := TList(T).t = struct
    let zero = []
    let plus = List.rev_append
    let (@@) = plus
    let concat = List.fold ~init:zero ~f:plus
  end
end


module String = struct
  module Builder : sig
    include S
    val add : t -> string -> t
    val run : t -> string
  end = struct
    module Base = struct
      type t = Rope.t
      let zero = Rope.of_string ""
      let plus = Rope.(^)
    end
    include Base
    include Make(Base)
    let add t x = Rope.(t ^ of_string x)
    let run = Rope.to_string
  end

  type t = string
  let zero = ""
  let plus = (^)
  let (@@) = (^)
  let concat xs = String.concat xs
end

module Set = struct
  module Make(S : Set.S) : S with type t := S.t = struct
    let zero = S.empty
    let plus = Set.union
    let (@@) = plus
    let concat = List.fold ~init:zero ~f:plus
  end
end

module List = struct
  module Make(T : T) : S with type t = T.t list = struct
    type t = T.t list
    let zero = []
    let plus = (@)
    let (@@) = (@)
    let concat = List.concat
  end
end

module Int = struct
  module Sum = struct
    type t = int
    include Make(struct
        type t = int
        let zero = 0
        let plus = (+)
      end)
  end
  module Product = struct
    type t = int
    include Make(struct
        type t = int
        let zero = 1
        let plus = ( * )
      end)
  end
  include Sum
end


module Float = struct
  module Sum = struct
    type t = float
    include Make(struct
        type t = float
        let zero = 0.
        let plus = (+.)
      end)
  end
  module Product = struct
    type t = float
    include Make(struct
        type t = float
        let zero = 1.
        let plus = ( *. )
      end)
  end
  include Sum
end
