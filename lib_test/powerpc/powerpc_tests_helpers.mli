open Core_kernel
open Bap.Std
open OUnit2

val nf : var
val pf : var
val zf : var
val ca : var
val ca32 : var
val lr  : arch -> var
val ctr : arch -> var
val tar : arch -> var
val cri : var Int.Map.t
val mem : arch -> var

module E : sig
  val cr : exp
  val cri : exp Int.Map.t
end


(** [cr_bit n] - returns a condition register bit [n] *)
val cr_bit : int -> var

(** [find_gpr arch name] - return a GPR with a [name] *)
val find_gpr : arch -> string -> var

(** [get_bil ?addr arch bytes] - returns a bil code
    from [bytes] for [arch]. [addr] is an instruction address,
    0 by default.  *)
val get_bil : ?addr:addr -> arch -> string -> bil Or_error.t

(** [check_gpr ?addr init_bil bytes var expected arch ctxt] -
    tests if a result bound to the [var] is equal to
    [exptected]. Evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
val check_gpr : ?addr:addr -> bil -> string -> var -> addr -> arch -> test_ctxt -> unit

(** [eval ?addr init_bil bytes arch] - evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
val eval : ?addr:addr -> bil -> string -> arch -> Bili.context
[@@warning "-D"]

(** [check_mem init bytes mem ~addr size expected ?endian arch ctxt] -
    tests if a word of [size] at [addr] in memory [mem] is equal to [expected].
    Evaluates bil, that is a concatenation of [init_bil] and code,
    obtained from lifting [bytes]. *)
val check_mem : bil -> string -> var -> addr:addr -> size:size -> addr -> ?endian:endian -> arch -> test_ctxt -> unit

(** [lookup_var context var] - returns a word, bound to [var] in [context] *)
val lookup_var : Bili.context -> var -> word option
[@@warning "-D"]

(** [make_bytes ws] - return a string of bytes, that obtained from
    concatenaion of [ws] *)
val make_bytes : word list -> string

(** [concat_words ws] - returns a concatenated [ws] *)
val concat_words : word list -> word

(** [is_equal_words w w'] - return true iff Some w = w' *)
val is_equal_words : word -> word option -> bool

(** [string_of_bytes bytes] - returns a readable string from [bytes] *)
val string_of_bytes : string -> string

(** [arch_width arch] - returns addr_size of [arch] in bits *)
val arch_width : arch -> int

(** PowerPC instruction forms  *)
type form = [
  | `B
  | `D
  | `I
  | `M
  | `MD
  | `MDS
  | `VA
  | `X
  | `XL
  | `XFX
  | `XO
  | `XS
]

(** [make_insn ~name form fields] - return a bytes sequence from
    instruction [form] and [fields] list. Raises is form and fields
    mismatches. If [name] is provided, checks that formed bytes
    belongs to expected instruction for [arch], raises if not.
    Default [arch] is ppc *)
val make_insn : ?name:string -> ?arch:arch -> form -> int list -> string
