module Sexp = Base.Sexp

type ('a,'b) comparator_t = ('a,'b) Base.Comparator.t
type ('a,'b) comparator = (module Base.Comparator.S
                            with type t = 'a
                             and type comparator_witness = 'b)
include Bitvec_sexp.Functions

module Ascending = struct
  type t = Bitvec.t
  include Base.Comparator.Make(struct
      type t = Bitvec.t
      include Bitvec_sexp.Functions
      let compare x y = Bitvec.compare x y [@@inline]
      let sexp_of_t = sexp_of_t
    end)
end

module Descending = struct
  type t = Bitvec.t
  include Base.Comparator.Make(struct
      type t = Bitvec.t
      include Bitvec_sexp.Functions
      let compare x y = Bitvec.compare y x [@@inline]
      let sexp_of_t = sexp_of_t
    end)
end

module Natural = Ascending

type ascending = Ascending.comparator_witness
type descending = Descending.comparator_witness
type natural = ascending

let ascending : (_,_) comparator = (module Ascending)
let descending : (_,_) comparator = (module Descending)
let natural = ascending

module Comparators = struct
  type bitvec_order = natural
  let bitvec_compare = Natural.comparator.compare
  let bitvec_equal x y = bitvec_compare x y = 0
  let bitvec_order = natural
  let bitvec_ascending = ascending
  let bitvec_descending = descending
end

let compare = Natural.comparator.compare

include Natural
