(** String computation detector.


    The detector uses maximum aposteriori likelihood estimator (MAP)
    to detect code that operates with textual data.

    We define textual data as a contigious blocks of characters from
    a specified alphabet.

    In our model a code is a sequence of instructions. Each
    instruction reads or writes characters. We're observing a set of
    characters accessed by an instruction (without any distinguishing
    whether it was a write or a read operation). The characters are
    produced by unknown values that are used during the
    computation. Some of these values are textual data. Any particular
    instruction either works with data produced by textual values or
    not. Moreover, we're looking for contigious sequences of
    instructions that work with textual value.

    The detector doesn't depend on a particular instruction
    implementation, instead it should be called after each instruction
    (or whatever granularity a user prefers) with a sequence of
    characters that were observed on that step.

    The detector distinguishes between two competing hypotheses. The
    null hypothesis is that we are observing a subsequence of
    instructions that doesn't interact with textual values. The
    alternative hypothesis states that the observed subsequence
    operates with one or more textual values.

    The detector operates in two different modes. In the search mode
    it looks for a subsequence that operates with textual values. Once
    it is found it switches to the second mode, and tries to find the
    end of this sequence, under an assumption that textual values have
    finite lengths. At the second mode we're switching hypotheses. We
    now assume as H0 a hypothesis that we're still processing textual
    values, and the H1 is that there are no longer textual
    values. Once we found the start and the end of the sequence, as
    well as all the characters that we attribute to the textual values
    the detection is considered finished.

    The detection relies on a fact, that when a code processes textual
    values, built from a specified alphabet, the characters from this
    alphabet occurs slightly more often, then the it is prescribed by
    the uniform distribution.

*)


open Core_kernel
open Format


(** [detector]  *)
type +'a t

type +'a decision




(** [create alphabet] creates a detector.

    @param alpha a desired probability of the false positive error;
    @param beta a desired probability of the false negative error;
    @param p1 posterior probability of the H1 hypothesis;
    @param ps probability of an alphabet character;
    @param len_pdf probability distribution function of textual values
           lengths.


*)
val create :
  ?alpha:float ->
  ?beta:float ->
  ?p1:float ->
  ?ps:float ->
  ?len_pdf:(float -> float) ->
  Char.Set.t -> 'a t


(** [run detector trace] runs a [detector] on a sequence on a [trace]
    represented as a sequence of bytes accessed during an exection.
    Returns a sequence of char sequences, where each subsequence is
    represented as a string and contains characters that were assumed
    to belong to the textual data.
*)

val run : 'a t -> ('a * char) Sequence.t -> 'a decision Sequence.t



(** [step t data char] performs one observation.*)
val step : 'a t -> 'a -> char -> 'a t


val decision : 'a t -> 'a decision option

val when_decided : 'a t -> f:('a decision -> 'b) -> 'b -> 'b

val abort : 'a t -> 'a decision option

val result : 'a decision -> ('a * char) list

val chars : 'a decision -> string

val data : ?rev:bool -> 'a decision -> 'a list



val pp : formatter -> 'a t -> unit

val pp_decision : formatter -> 'a decision -> unit

val pp_stats : formatter -> 'a t -> unit
