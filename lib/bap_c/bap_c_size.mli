(** An abstraction of sizeof operator.*)
open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_c_data
open Bap_c_type


type bits = Int.t


(** [next_multitude_of ~n x] returns [y >= x] so that [y]
    is a multitude of [n], i.e., [y = n * k].

    @since 2.5.0 *)
val next_multitude_of : n:int -> int -> int


(** [padding alignment offset] computes the required padding at
    [offset] to ensure the [alignment].

    @since 2.5.0 *)
val padding : Size.t -> int -> int



(** [max_enum_elt enum] returns the maximum element in the enum
    specification.

    @since 2.5.0
*)
val max_enum_elt : (string * int64 option) list -> int64

(** The base class for computing sizes and aligments of C data types.

    The algorithm is implemented as a class to allow
    a particular implementation to fine tune the calculation.

    The [model] argument defines the default sizes for integral data
    types. If no suitable model is available for your architecture
    then use the closest model and override the specific methods to
    fine-tune the data model of your target.

    The entry methods are [bits] and [aligment].

    {3 Example}

    For example, let's compute the size of the

    {v
      struct foo {
         char v1;
         int  v2;
         char v3;
      };
    v}

    Using the LP64 data model, in which integers are 32 bit long and
    char is 8 bit. The size of the structure is 12 bytes, due to
    the 3 bytes of padding before [v2] and six bytes of trailing
    padding.

    {[
      # let size = new C.Size.base `LP64;;
      # size#bits C.Type.(structure "foo" [
          "v1", basic `char;
          "v2", basic `uint;
          "v3", basic `char
        ]);;
      - : C.Size.bits option = Some 96
    ]}
*)
class base :  model -> object


    (** returns a size of the data type representation in bits.

        For incomplete types returns [None]. The size is always a
        multitude of the data type alignment and includes the
        paddings necessary for preserving the alignment restrictions.

        @since 2.5.0 the size is a multitude of the alignment.
    *)
    method bits : t -> bits option

    (** [alignment t] the alignment of data type [t].

        The alignment of
        - void or an incomplete type is 8;
        - a scalar is [sizeof(t)];
        - an array is the alignment its element;
        - a function pointer is [sizeof] the pointer;
        - a structure or a union is the largest of the element's alignments.

    *)
    method alignment : t -> size

    (** DEPRECATED. Use the [padding] function if you need to compute
        padding.  *)
    method padding : t -> bits -> size option
    [@@deprecated "since [2021-05] this method is ignored"]
    (* this method was deprecated as
       1) it has an incorrect type (padding can have any number of bits)
       2) padding is fully defined by the alignemnt and there is no
          need to parameterize it. *)


    (** [array spec] if array [spec] is complete, i.e., the number of
        elements is known, then returns a product of the
        array size and the array's element type in bits,
        otherwise returns [None]
    *)
    method array : (cvr qualifier, array) spec -> bits option

    (** if spec is complete then returns a size in bits of the biggest
        element, including the padding between fields, but excludeing
        the trailing padding. *)
    method union : (no_qualifier, compound) spec -> bits option

    (**  if spec is complete then returns a total size of the
         structure, including the padding between fields, but excluding
         the trailing padding. *)
    method structure : (no_qualifier, compound) spec -> bits option


    (** the size of intergral types.  *)
    method integer : integer -> size

    (** the size of a pointer.  *)
    method pointer : addr_size

    (** the size of the enumeration.  *)
    method enum : (string * int64 option) list -> size

    (** the size of a real floating-point data type.  *)
    method real : real -> [`r32 | `r64 | `r128]

    (** the size of a complex floating-point data type.  *)
    method complex : complex -> size

    (** the size of a floating-point data type.  *)
    method floating : floating -> size

    (** the size of a basic data type.  *)
    method basic : basic -> size

    (** the size of a scalar data type.  *)
    method scalar : scalar -> size
  end
