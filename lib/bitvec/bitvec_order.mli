(** Provides comparators for use with Janestreet Libraries.

    A comparator is a comparison function paired with a type that
    is unique to this function and that acts as the witness of
    equality between two comparison functions.

    Comparators are extensively used in the Janestreet's suite of
    libraries, e.g., Base, Core_kernel, Core, etc. Comparators are
    used to create sets, maps, hashtables, to instantiate interfaces,
    that require comparison, and, in general, for algorithms that
    require comparison.

    This module provides two comparators, [ascending] and
    [descending], which represent two corresponding orderings, as
    well as the [natural] comparator, which is the default ordering
    that equals with the [ascending] order, which should be used in
    cases where a particular ordering doesn't matter.

    This library interface is designed from the point of view of the
    library user, to minimize verbosity and maximize readability and
    the ease of use.

    For example, an empty set is created as

    {[let words = Set.empty Bitvec_order.natural]},

    and it has type

    {[(Bitvec.t,Bitvec_order.natural) set]}

    and the [compare] function could be accessed directly, as

    {[Bitvec_order.ascending.compare]}.

    This module also provides (implements) the [Base.Comparator.S]
    interface that is required by a few Janestreet functors, in
    particular, you can construct the type of a set of bitvectors,
    as

    {[type t = Set.M(Bitvec_oder)]}, or for a mapping from
    bitvectors to OCaml [int] it would be

    {[type t = int Map.M(Bitvec_order)]}


    And to instantiate the [Comparable.S] interface,

    {include Comparable.Make(Bitvec_order)}.

    See also, [Bitvec_binprot] that provide support for
    the binable interfaces.

    Finally, for even more concise and readable syntax we provide
    the [Comparators] module that designed to be opened, since it
    defines properly prefixed identifiers, which will unlikely clash
    with any existing name, so that the above examples could be
    expressed

    {[
      open Bitvec_order.Comparators
      let words = Set.empty bitvec_order
      type words = (bitvec, bitvec_order) Set.t
      let decreasing x y = bitvec_descending.compare x y
    ]}
*)
type t = Bitvec.t [@@deriving compare, sexp]


type ascending
type descending
type natural = ascending

val natural : (t, natural) Base.Comparator.t
val ascending : (t, ascending) Base.Comparator.t
val descending : (t, descending) Base.Comparator.t

module Natural : Base.Comparator.S
  with type t = t
   and type comparator_witness = natural

module Ascending : Base.Comparator.S
  with type t = t
   and type comparator_witness = ascending

module Descending : Base.Comparator.S
  with type t = t
   and type comparator_witness = descending

include Base.Comparator.S
  with type t := t
   and type comparator_witness = natural


module Comparators : sig
  type bitvec_order = natural
  val bitvec_compare : t -> t -> int
  val bitvec_equal : t -> t -> bool
  val bitvec_order  : (t, natural) Base.Comparator.t
  val bitvec_ascending  : (t, ascending) Base.Comparator.t
  val bitvec_descending : (t, descending) Base.Comparator.t
end
