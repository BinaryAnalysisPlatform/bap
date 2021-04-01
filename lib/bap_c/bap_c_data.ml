(** C Data model.

    This module defines abstractions for C values.

    A value is backed by a datum - a sequence of bits that represents
    the value. This module also defines models for integer
    representation.
*)
open Core_kernel
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

(** Abstract value lattice. The lattice is complete, and
    [Set []] is the supremum, i.e., the bot.*)
type value =
  | Top
  (** any possible value  *)
  | Set of word list
  (** one of the specified *)
[@@deriving bin_io, compare, sexp]


(** abstraction of a С datum.

    The datum is a sequence of bits that represenst a particular C
    value. We abstract datum as either an immediate value of the given
    size and value lattice, or a sequence of data, or a pointer to a
    datum.*)
type t =
  | Imm of Size.t * value
  (** [Imm (size,value)] *)
  | Seq of t list
  (** [Seq (t1,..,tN)]   *)
  | Ptr of t
  (** [Ptr (type,size)]  *)
[@@deriving bin_io, compare, sexp]


(**  *)
