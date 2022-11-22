(** C Data model.

    This module defines abstractions for C values.

    A value is backed by a datum - a sequence of bits that represents
    the value. This module also defines models for integer
    representation.
*)
open Core_kernel[@@warning "-D"]
open Bap.Std

(** models for 32 bit systems  *)
type model32 = [
  | `LP32
  | `ILP32
]

(** models for 64 bit systems  *)
type model64 = [
  | `ILP64
  | `LLP64
  | `LP64
]


(** The following table summarize all models of integer
    representation.

    {v
       LP32	 ILP32	 ILP64	 LLP64	 LP64
 char	  8	    8	     8	     8	    8
 short	 16	   16	    16	    16	   16
 int	 16	   32	    64	    32	   32
 long	 32	   32	    64	    32	   64
 addr    32	   32	    64	    64	   64
 v}
*)
type model = [model32 | model64]

(** A value lattice.*)
type value =
  | Top (** any possible value  *)
  | Set of word list (** one of the specified, [Set []] is bot *)
[@@deriving bin_io, compare, sexp]


(** A C Object representation.

    The type is parameterized with the object layout representation to
    enable the recursive definition of the generalized layout type.

    @since 2.5.0 *)
type 'd obj =
  | Basic of Bap_c_type.basic   (** A value of a basic type *)
  | Field of (string * 'd)       (** A struct or union field *)
  | Undef                       (** Undefined data (padding or code) *)
  | Union of 'd list             (** Union of values  *)
[@@deriving bin_io, compare, sexp]

(** abstraction of a С datum.

    The datum is a sequence of bits that represents a C object. We
    abstract datum as either an immediate value of the given size,
    or a sequence of data, or a pointer to a datum.

    @since 2.5.0
*)
type ('d,'s) datum =
  | Imm of 's * 'd             (** [Imm (size, value)] *)
  | Seq of ('d,'s) datum list  (** [Seq [t1; ... ;tN]] *)
  | Ptr of ('d,'s) datum       (** [Ptr datum]        *)
[@@deriving bin_io, compare, sexp]


(** Describes C object's layout.  *)
type layout = {layout : (layout obj,int) datum}
[@@deriving bin_io, compare, sexp]


(** The datum that uses value lattice for object representation. *)
type t = (value,Size.t) datum
[@@deriving bin_io, compare, sexp]


(** [pp ppf datum] prints the datum in a human-readable form.
    @since 2.5.0 *)
val pp : Format.formatter -> t -> unit


(** [pp_layout ppf layout] outputs layout in a human-readable form.
    @since 2.5.0 *)
val pp_layout : Format.formatter -> layout -> unit
