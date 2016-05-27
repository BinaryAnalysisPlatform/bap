open Core_kernel.Std
open Bap.Std
(**
   [{v
       LP32	 ILP32	 ILP64	 LLP64	 LP64
 char	  8	    8	     8	     8	    8
 short	 16	   16	    16	    16	   16
 int	 16	   32	    64	    32	   32
 long	 32	   32	    64	    32	   64
 addr    32	   32	    64	    64	   64
 v}]

*)

type model32 = [
  | `LP32
  | `ILP32
]

type model64 = [
  | `ILP64
  | `LLP64
  | `LP64
]

type model = [model32 | model64]

(** Abstract value lattice. The lattice is complete, and
    [Set []] is the supremum, i.e., the bot.*)
type value =
  | Top                        (** any possible value  *)
  | Set of word list            (** one of the  *)
  [@@deriving bin_io, compare, sexp]


(** abstraction of a С datum.
    A datum is either an immediate value of the given size and value
    lattice, or it is a pointer to datum.
*)
type t =
  | Imm of Size.t * value       (** {Imm (size,value)}  *)
  | Seq of t list
  | Ptr of t            (** {Ptr (type,size)}   *)
  [@@deriving bin_io, compare, sexp]
