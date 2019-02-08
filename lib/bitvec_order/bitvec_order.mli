(** Provides comparators for use with Janestreet Libraries.

    A comparator is a comparison function paired with a type that
    is unique to this function and that acts as the witness of
    equality between two comparison functions.

    Comparators are extensively used in the Janestreet's suite of
    libraries, e.g., Base, Core_kernel, Core, etc. Comparators are
    used to create sets, maps, hashtables, to instantiate interfaces,
    and algorithms that require comparison.

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

    (See the note on comparators below for the old style comparators)

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

    {2 The old style comparators}

    Historically, the comparator in the Janestreet libraries had two
    representations - as a record that contains the [compare]
    function, and as a module that contains this record and an
    instance of the witness type. The former representation is mostly
    used for internal purposes, while the latter is expected in most
    of the public functions, like [Set.empty], [Map.empty] in the form
    of the first-class module, or in different [Make*] functors. In
    this library, when we use the word comparator to refer to the
    latter representation, i.e., to the module. When the underlying
    comparator record is needed, it could be obtained through the
    [comparator] field of the module.

    The older versions of Core and Base libraries were accepting the
    comparator as a record, so instead of writing

    {[Set.empty Bitvec_order.natural]}

    the following notation should be used

    {[Set.empty Bitvec_order.Natural.comparator]}
*)
type t = Bitvec.t


(** [compare x y] orders [x] and [y] in the natural order.  *)
val compare : t -> t -> int


(** type index for the increasing order  *)
type ascending

(** type index for the decreasing order  *)
type descending


(** we use the increasing order as the natural ordering  *)
type natural = ascending

(** a type abbreviation for a comparator packed into a module.

    See the note about the historical meaning of the word comparator.
*)
type ('a,'b) comparator = (module Base.Comparator.S
                            with type t = 'a
                             and type comparator_witness = 'b)


(** [natural] the packed comparator that sorts in the natural order *)
val natural : (t, natural) comparator

(** [ascending] the packed comparator that sorts in the increasing order *)
val ascending : (t, ascending) comparator

(** [descending] the packed comparator that sorts in the decreasing order  *)
val descending : (t, descending) comparator

(** [natural] the comparator that sorts in the natural order  *)
module Natural : Base.Comparator.S
  with type t = t
   and type comparator_witness = natural

(** [natural] the comparator that sorts in the natural order  *)
module Ascending : Base.Comparator.S
  with type t = t
   and type comparator_witness = ascending

(** [natural] the comparator that sorts in the natural order  *)
module Descending : Base.Comparator.S
  with type t = t
   and type comparator_witness = descending

(** provides the natural order by default  *)
include Base.Comparator.S
  with type t := t
   and type comparator_witness = natural


(** Open this module to make the following fields available  *)
module Comparators : sig

  (** the default ordering for bitvectors  *)
  type bitvec_order = natural


  (** [bitvec_compare x y] orders [x] and [y] in the natural order  *)
  val bitvec_compare : t -> t -> int


  (** [bitvec_equal x y] is true if [x] is equal to [y]. *)
  val bitvec_equal : t -> t -> bool


  (** [natural] the packed comparator that sorts in the natural order *)
  val bitvec_order  : (t, natural) comparator

  (** [ascending] the packed comparator that sorts in the increasing order *)
  val bitvec_ascending  : (t, ascending) comparator

  (** [descending] the packed comparator that sorts in the decreasing order  *)
  val bitvec_descending : (t, descending) comparator
end
