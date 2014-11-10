open Core_kernel.Std
open Bap_types.Std

type 'a t
type mem = Bap_memory.t
type 'a hashable = 'a Hashtbl.Hashable.t

(** creates an empty table  *)
val empty : 'a t

(** [add table mem v] returns a new table with added mapping from a
    mem region [mem] to a data value [v] *)
val add : 'a t -> mem -> 'a -> 'a t Or_error.t

(** returns a new table with all mappings from the mem region
    [mem] removed *)
val remove : 'a t -> mem -> 'a t

(** [change tab mem ~f] function [f] is applied to a set of all memory
    regions that intersects with [mem]. If function [f] evaluates to
    [`remap y] then all memory regions that have had intersections
    with [mem] will be removed from the new map and memory region
    [mem] will be mapped to [y]. If [f] evaluates to [`remove], then
    the regions will be removed, and nothing will be added. If it
    evaluates to [`skip] then the table will be returned unchanged.
    Intersections are passed sorted in an ascending order.
*)
val change : 'a t -> mem -> f:((mem * 'a) seq -> [
    | `remap of 'a               (** change value  *)
    | `remove                   (** remove all  *)
    | `skip])                   (** bail out  *)
  -> 'a t

(** [length table] returns a number of entries in the table  *)
val length : 'a t -> int

(** [find table mem] finds an element mapped to the memory region [mem]  *)
val find : 'a t -> mem -> 'a option

(** [intersections table mem] returns all mappings in a [table] that
    have intersections with [mem] *)
val intersections : 'a t -> mem -> (mem * 'a) seq

(** [fold_intersections table mem] folds over all regions
    intersecting with [mem] *)
val fold_intersections : 'a t -> mem -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b

(** [mem table mem] is true if table contains mem region [mem]  *)
val mem : _ t -> mem -> bool

(** [next table elt] returns element next to [elt], if any *)
val next : 'a t -> mem -> (mem * 'a) option

(** [next table elt] returns element preceding to [elt], if any *)
val prev : 'a t -> mem -> (mem * 'a) option

(** [min tab] return the lowest binding  *)
val min : 'a t -> (mem * 'a) option

(** [max tab] return the highest binding  *)
val max : 'a t -> (mem * 'a) option

(** Relation multiplicity.
    For a given type ['a] creates type ['m]
*)
type ('a,'m) r

(** {4 Table relations}  *)

(** [0..*]  *)
val many : ('a, 'a seq) r

val at_least_one : ('a, 'a * 'a seq) r

(** [1..1]     *)
val one : ('a, 'a) r

(** [0..1]  *)
val maybe_one : ('a, 'a option) r


(** [link relation t t1 t2] takes two tables and returns a mapping
    from elements of one table to elements of other table.

    Parameter [t] specifies a [hashable] typeclass of the type ['a]. If
    type ['a] implements [Hashable] interface, then you can obtain it
    with [hashable] function, e.g. [Int.hashable] with return the
    appropriate type class. If ['a] doesn't implement [Hashable], then
    it can be implemented manually.

    Relation specifies the multiplicity of the relation between
    entities from table [t1] to entities from table [t2], and is
    summarized below:

    - [one_to_many] means that a particular region from table [t1] can
      span several memory regions from table [t2]. Example: segments
      to symbols relation.

    - [one_to_one] means that for each value of type ['a] there is
      exactly one value of type ['b]. This relation should be used with
      caution, since it is quantified over _all_ values of type
      ['a]. Indeed, it should be used only for cases, when it can be
      guaranteed, that it is impossible to create such value of type
      ['b], that has no correspondence in table [t2]. Otherwise,
      [one_to_maybe_one] relation should be used. Example: llvm
      machine code to assembly string relation.

    - [one_to_maybe_one] means that for each value in table [t1] there
      exists at most one value in table [t2]. Example: function to
      symbol relation.


    {5 Examples}
    {[
      let mc_of_insn  = link one_to:one Insn.hashable insns mcs
      let syms_of_sec = link one_to:many Sec.hashable  secs syms
    ]} *)

val link : one_to:('b,'r) r -> 'a hashable -> 'a t -> 'b t -> 'a -> 'r


(** [rev_map arity t tab] creates a reverse mapping from values of
    typeclass [t] stored in table [tab] to memory regions.

    Note. not every mapping is reversable, for example, trying to obtain
    a reverse of surjective mapping will result in error. Although,
    surjective mappings can be mapped using [~one_to:many] mapping. A
    particular example of surjective mapping is [symbol] tables, in a
    case when functions can occupy several non-contiguous regions of
    memory.

    { 5 Examples}

    To create a mapping from a function symbol to sequence of memory
    regions with it code:

    {[rev_map one_to:many Sym.hashable tab]}

*)
val rev_map : one_to:(mem,'r) r -> 'a hashable -> 'a t -> ('a -> 'r) Or_error.t

(** {3 Iterators}

    This section provides a common set of iterators. Note: name
    iterator is used in a functional meaning, i.e., an iterator is a
    function that takes a data structure and another function, and
    applies it to all elements in some manner.

    All iterators share some common part of interface that was lifted
    to a ['a ranged] type. When you see

    [('a t -> f:('a -> bool) -> bool) ranged]

    just mentally substitute it with:

    [?start -> ?until -> 'a t -> f:('a -> bool) -> bool].

    In other words ['f ranged] just prepends [?start -> ?until ->] to
    function with type ['f] (do not forget that ['f] can be an arrow
    type).

    [start] and [until] parameters allows to narrow iteration to some
    subset of table. If they are unspecified then iteration would be
    performed on all table entries in an ascending order of
    addresses. If they are specified, then if [start <= until], then
    iteration will be performed in the same order but on a specified
    subset. In the case, when [start > until], iteration will be
    performed in a decreasing order.
*)
type 'a ranged
  = ?start:mem   (** defaults to the lowest mapped region *)
  -> ?until:mem   (** defaults to the highest mapped area  *)
  -> 'a

val exists   : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
val for_all  : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
val exists   : ('a t -> f:(      'a -> bool) -> bool) ranged
val for_all  : ('a t -> f:(      'a -> bool) -> bool) ranged

val count    : ('a t -> f:('a -> bool) -> int) ranged
val find_if  : ('a t -> f:('a -> bool) -> 'a option) ranged
val find_map : ('a t -> f:('a -> 'b option) -> 'b option) ranged
val fold  : ('a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b) ranged
val iter  : ('a t -> f:('a -> unit) -> unit) ranged

val find_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b option) ranged
val foldi: ('a t -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b) ranged
val iteri : ('a t -> f:(mem -> 'a -> unit) -> unit) ranged

val map : ('a t -> f:('a -> 'b) -> 'b t) ranged
val mapi : ('a t -> f:(mem -> 'a -> 'b) -> 'b t) ranged

(** removes all mappings that do not satisfy the predicate  *)
val filter : ('a t -> f:('a -> bool) -> 'a t) ranged
val filter_map : ('a t -> f:('a -> 'b option) -> 'b t) ranged

val filteri : ('a t -> f:(mem -> 'a -> bool) -> 'a t) ranged
val filter_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b t) ranged


(** [to_sequence_in tab] converts the table [t] to a
    sequence of key-value pairs.  *)
val to_sequence : ('a t -> (mem * 'a) Sequence.t) ranged


(** [regions table] returns in an ascending order of addresses all
    memory regions mapped in a [table] *)
val regions : ('a t -> mem seq) ranged

(** [regions table] returns in an ascending order of addresses all
    elements mapped in a [table] *)
val elements : ('a t -> 'a seq) ranged
