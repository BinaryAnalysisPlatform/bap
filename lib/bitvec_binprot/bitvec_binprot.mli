open Bin_prot

(** Provides serialization functions for the Binprot Protocol.*)


include Binable.S with type t = Bitvec.t


(** Same module, but functions only without the type.

    Useful, for extending an existing interface with the binable
    interface, without hitting the same type defined twice error,
    e.g.

    {[include Comprable.Make_binable_using_comparator(struct
        include Bitvector_order
        include Bitvector_binprot.Functions
      end]}
*)
module Functions : Binable.S with type t := Bitvec.t
