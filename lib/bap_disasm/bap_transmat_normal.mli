open Bap.Std
open Bigarray

type t
type corpus = insn memmap
type mat = (float, float64_elt, c_layout) Array2.t

(** of_arch is the main and only constructor here, and naturally *)
    (** it does the job of retrieving from a retained data
    store the statistics specific to an architecture *)
val of_arch : arch -> t

(** a constructor identical to of_arch, except that it does not *)
    (** perform the actual probability calculation. *)
val totals_of_arch : arch -> t

(** transition accepts a training data matrix and two instructions *)
    (** and subsequently returns the corresponding statistical
    likelihood of transition. *)
val transition : t -> insn -> insn -> float

(** update_from_shingles accepts a corpus, which is a collection of instructions *)
    (** These instructions are examined to determine static
    successors, and for each static successor it updates the
    statistic accordingly. *)
val update_from_shingles : t -> corpus -> unit

(** A convenience function that facilitates the translation from *)
    (** disasm to a memmap; it updates the training data *)
val update_from_disasm : t -> disasm -> unit

(** transition_of_data computes the transition matrix when given a
    totals and then calculates and returns transition probabilities
    using it
*)
val transition_of_data : mat -> mat
