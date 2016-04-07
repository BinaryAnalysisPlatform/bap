open Core_kernel.Std
open Bap.Std
open Bap_c_type
open Bap_c_data


type bits = Int.t option

(** Base class for computing size of C data types.
    The algorithm is implemented as a class to allow
    a particular implementation to fine tune the calculation.
    We need here an open recursion, since type is inherently
    recursive.

    The entry method is the [bits] method.
*)
class base : size -> model -> object
    (** returns a size of the data type representation if type
        definition is complete. Otherwise [None] is returned.
        The size is computed with respect to padding and alignment
        restructions.
    *)
    method bits : t -> bits

    (** [alignemt t] calculates an alignment restriction for data
        type [t]. The default implementation is equal to min of
        bitwidth of the type t and bitwidth of a pointer.   *)
    method alignment : t -> Int.t

    (** [padding t off] computes a required padding at given offset
        that should be inserted before value of type [t] to satisfy
        the aligment restriction for [t], as determined by the
        [alignement] method.  *)
    method padding : t -> Int.t -> Int.t


    (** [array spec] if array [spec] is complete, then returns a
        product of the bitwidth of array size and array's element
        type, otherwise returns [None] *)
    method array : (no_qualifier, t * Int.t option) spec -> bits

    (** returns   *)
    method union : (no_qualifier, t list) spec -> bits
    method structure : (no_qualifier, t list) spec -> bits


    method integer : integer -> size
    method enum : Int.t -> size
    method real : real -> [`r32 | `r64 | `r128]
    method complex : complex -> size
    method floating : floating -> size
    method basic : basic -> size
    method scalar : scalar -> size
  end
