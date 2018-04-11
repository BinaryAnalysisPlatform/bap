(** An abstraction of sizeof operator.*)
open Core_kernel
open Bap.Std
open Bap_c_data
open Bap_c_type


type bits = Int.t

(** Base class for computing size of C data types.
    The algorithm is implemented as a class to allow
    a particular implementation to fine tune the calculation.
    We need here an open recursion, since type is inherently
    recursive.

    The entry method is the [bits] method.
*)
class base :  model -> object
    (** returns a size of the data type representation if type
        definition is complete. Otherwise [None] is returned.
        The size is computed with respect to padding and alignment
        restructions.
    *)
    method bits : t -> bits option

    (** [alignment t] calculates an alignment restriction for data
        type [t]. The default aligment rules are the following:
        - if type is scalar then the alignment is [sizeof(t)];
        - if type is [elt\[\]] then the alignment is [sizeof(elt)];
        - if type is structure or union, the the alignment of is
          the maximum alignment of a field;
        - if type is function, then aligment is equal to sizeof
        pointer
        - if type is void then alignment is 8 bits.*)
    method alignment : t -> size

    (** [padding t off] computes a required padding at given offset
        that should be inserted before value of type [t] to satisfy
        the aligment restriction for [t], as determined by the
        [alignment] method.  *)
    method padding : t -> bits -> size option


    (** [array spec] if array [spec] is complete, then returns a
        product of the bitwidth of array size and array's element
        type, otherwise returns [None] *)
    method array : (cvr qualifier, array) spec -> bits option

    (** if spec is complete then returns a size of the biggest
        element, including padding *)
    method union : (no_qualifier, compound) spec -> bits option

    (**  if spec is complete then returns a total size of the
    structure, including padding. *)
    method structure : (no_qualifier, compound) spec -> bits option

    method integer : integer -> size
    method pointer : addr_size
    method enum : (string * int64 option) list -> size
    method real : real -> [`r32 | `r64 | `r128]
    method complex : complex -> size
    method floating : floating -> size
    method basic : basic -> size
    method scalar : scalar -> size
  end
