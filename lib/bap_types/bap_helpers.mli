open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor

val find_map : 'a #finder -> bil -> 'a option
val find : unit #finder -> bil -> bool
val iter : unit #visitor -> bil -> unit
val fold : 'a #visitor -> init:'a -> bil -> 'a
val map : #mapper -> bil -> bil

(** [is_referenced x p] is [true] if [x] is referenced in some expression or
    statement in program [p] *)
val is_referenced : var -> bil -> bool

(** [is_assigned x p] is [true] if there exists such [Move]
    statement, that [x] occures on the left side of it. If [strict]
    is true, then only unconditional assignments. By default,
    [strict] is [false] *)
val is_assigned : ?strict:bool -> var -> bil -> bool

(** [prune_unreferenced p] remove all assignments to variables that
    are not used in the program [p] *)
val prune_unreferenced : bil -> bil

(** [normalize_negatives p] transform [x + y] to [x - abs(y)] if [y < 0] *)
val normalize_negatives : bil -> bil

(** [substitute x y p] substitutes each occurrence of expression [x] by
    expression [y] in program [p] *)
val substitute : exp -> exp -> bil -> bil


(** [substitute_var x y p] substitutes all occurences of variable [x]
    by expression [y] *)
val substitute_var : var -> exp -> bil -> bil

(** [fold_consts] evaluate constant expressions.
    Note: this function performs only one step, and has no loops,
    it is supposed to be run using a fixpoint combinator.
*)
val fold_consts : bil -> bil

(** [constant_folder] is a class that implements the [fold_consts]  *)
class constant_folder : mapper

(** [fixpoint f] applies transformation [f] until fixpoint is
    reached. If the transformation orbit contains non-trivial cycles,
    then the transformation will stop at an arbitrary point of a
    cycle. *)
val fixpoint : (bil -> bil) -> (bil -> bil)

(** Bil provides two prefix tries trees.

    The default one is not normalized and will compare bil statements
    literally. This means that comparison is sensitive to variable
    names and immediate values. Depending on your context it may be
    find or not. For example, two [SP] variables may compare as different
    if one of them was obtained from different compilation (and met
    the other one through some persistant storage, e.g., file on hard
    disk). Moreover, BIL obtained from different lifters will have
    different names for the same registers. All this issues are
    addressed in normalized [Trie].
*)
module Trie : sig
  type normalized_bil

  (** [normalize ?subst bil] normalize BIL. If [subst] is provided,
      then substitute each occurence of the fst expression to the
      snd expression before the normalization. The effect of
      normalization is the following:

      1. All immediate values are compared equal
      2. All variables are compared nominally
      3. BIL is simplified to reduce the syntactic differences
      (but the comparison is still syntactic, and (x + 2) will
      be compared differently to (2 + x).
  *)
  val normalize : ?subst:(exp * exp) list -> bil -> normalized_bil

  module Normalized : Trie with type key = normalized_bil
  include Trie with type key = bil
end
