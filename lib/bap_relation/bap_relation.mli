(** A representation of relations between two sets.

    A relation between two sets is a set of pairs made from the
    elements of these sets. The precise mathematical defition is given
    below. This module implements a bidirectional mapping between two
    sets and computes their matching that defines bijections between
    the sets.

    {2 Format Definition and Notation}

    Given two sets [K] and [S], with meta-variables [x,y,z] ranging
    over [K] and meta-variables [r,s,t] ranging over [S] we will
    denote a finitary relation [R] as a subset of the cartesian
    product [K x S], which is a set of pairs [(x,r), ..., (z,t)],
    which we represent as a bipartite graph [G = (K,S,R)].
*)

(** the type for relation between ['k] and ['s].  *)
type ('k,'s) t

(** [empty compare_k compare_s] the empty relation between two sets.

    - [compare_k] is the function that defines order of the elements
      of the set [K].

    - [compare_s] is the function that defines order of the elements
      of the set [S].

    {3 Example}
    {[
      let empty = Bap_relation.empty
          Int.compare
          String.compare
    ]}
*)
val empty : ('k -> 'k -> int) -> ('s -> 's -> int) -> ('k,'s) t

(** [is_empty rel] is true if the relation [rel] is an empty set.  *)
val is_empty : (_,_) t -> bool

(** [add relation x s] establishes a relation between [x] and [s].  *)
val add : ('k,'s) t -> 'k -> 's -> ('k,'s) t

(** [mem rel x s] is [true] if [(k,s)] is in the relation [rel]. *)
val mem : ('k,'s) t -> 'k -> 's -> bool

(** [findl rel x] finds all pairs in [rel] that have [x] on the left.  *)
val findl : ('k,'s) t -> 'k -> 's list

(** [findr rel s] finds all pairs in [rel] that have [s] on the right.  *)
val findr : ('k,'s) t -> 's -> 'k list

(** [fold rel init f] folds over all pairs in the relation [rel].  *)
val fold : ('k,'s) t -> init:'a -> f:('k -> 's -> 'a -> 'a) -> 'a

(** [iter rel f] iterates over all pairs in the relation [rel].   *)
val iter : ('k,'s) t -> f:('k -> 's -> unit) -> unit


(** {2 Bijections and matching}

    The set of independent edges [M] (the matching) of the graph [G]
    forms a finite bijection between [K] and [S]. It is guaranteed
    that for each pair [(x,s)] in [M] there is no other pair in [M],
    that will include [x] or [s].

    Edges [R] that are not in the matching [M] represent a subset of
    [R] that do not match because of one the two anomalies:
    - A non-injective forward mapping occurs when the same value from
      the set [S] is in relation with more than one value from the set
      [K], e.g., [(x,s), (y,s)] is encoded as
      [Non_injective_fwd ([x,y],s)];

    - A non-injective backward mapping occurs when the same value from
      the set [K] is in relation with more than one value from the set
      [S], e.g., [(x,r), (x,s)] is encoded as
      [Non_injective_bwd ([r;s],x);
*)

(** the reason why was the pair left unmatched  *)
type ('k,'s) non_injective =
  | Non_injective_fwd of 'k list * 's (** Non-injective forward mapping.  *)
  | Non_injective_bwd of 's list * 'k (** Non-injective backward mapping.  *)

(** [matching relation data] computes the matching for the given [relation].

    Calls [saturated x s data] for each [(x,s)] in the matching
    [M] (see the module description) and [unmatched z reason d] for
    each [(z,t)] in the relation that are not matched, the reason
    is one of the:

    - [Non_injective_fwd (xs,s)] if the mapping [K -> S] that is
      induced by the [relation] is non-injective, because the set of
      values [xs] from [K] are mapped to the same value [s] in [S].

    - [Non_injective_bwd (ss,x)] if the mapping [S -> K] that is
      induced by the [relation] is non-injective, because the set of
      values [ss] from [S] are mapped to the same value [x] in [K].
*)
val matching : ('k,'s) t ->
  ?saturated : ('k -> 's -> 'a -> 'a) ->
  ?unmatched : (('k,'s) non_injective -> 'a -> 'a) -> 'a -> 'a
