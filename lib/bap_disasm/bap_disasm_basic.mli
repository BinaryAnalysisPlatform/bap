(** Basic disassembler.

    This is a target agnostic basic disassembler. It
*)

open Core_kernel.Std
open Bap_types.Std

(** {2 Imported types}  *)
type mem = Bap_memory.t with sexp_of
type kind = Bap_insn_kind.t with compare, sexp

(** predicate to drive the disassembler *)
type pred = [
  | `valid  (** stop on first valid insn  *)
  |  kind   (** stop on first insn of the specified kind *)
] with sexp

(** {2 Basic types }  *)
type reg  with bin_io, compare, sexp
type imm  with bin_io, compare, sexp
type fmm  with bin_io, compare, sexp
type (+'a,+'k) insn
type (+'a,+'k) insns = (mem * ('a,'k) insn option) list



(** Disassembler.

    ['a] and ['k] type variables specify disassembler modes of
    operation. In a process of disassembly it can store extra
    information that might be useful. Although, since storing it
    takes extra time and space, it is disabled by default.

    The first type variable specifies whether storing assembly strings
    is enabled. It can be switched using [store_asm], [drop_asm]
    functions. When it is enabled, then this type variable will be set
    to [asm], and it will give an access to functions that returns
    this information. Otherwise, this type variable will be set to
    [empty], thus stopping you from accessing assembler information.

    The second type variable stands for [kinds], i.e. to store or not to
    store extra information about instruction kind.

    Note: at some points you can have an access to this information
    even if you don't enable it explicitely.
*)
type ('a,'k) t

type empty     (** set when information is not stored                *)
type asm       (** set when assembler information is stored        *)
type kinds     (** set when instruction kind information is stored *)


(** Disassembler state.

    Words of precautions: this state is valid only inside handlers
    functions of the [run] function. It shouldn't be stored
    anywhere.
    First two type variables are bound correspondingly to two
    variables of the disassmbler [('a,'k) t] type. Third type variable,
    is for user data type, that can be used to pass extra information
*)
type (+'a,+'k,'s,'r) state


(** [create ?debug_level ?cpu ~backend target] creates a disassembler
    for the specified [target]. All parameters are backend specific,
    consult the concrete backend for more information. In general,
    the greater [debug_level] is, the more debug information will be
    outputed by a backend. To silent backend set it [0]. This is a
    default value. Example:

    [create ~debug_level:3 ~backend:"llvm" "x86_64"]
*)
val create :
  ?debug_level:int ->
  ?cpu:string ->
  backend:string ->
  string ->
  (empty, empty) t Or_error.t

(** enables storing assembler information  *)
val store_asm : (_,'k) t -> (asm,'k) t

(** enables storing instruction kinds information *)
val store_kinds : ('a,_) t -> ('a,kinds) t

(** disables storing assembler information  *)
val drop_asm  : (_,'k) t -> (empty,'k) t

(** disables storing instruction kinds information *)
val drop_kinds : ('a,_) t -> ('a,empty) t

(** [run ?stop_on ?invalid ?stopped dis mem ~init ~return ~hit]
    performs recursive disassembly of specified memory [mem]. The
    process of disassembly can be driven using [stop], [step], [back]
    and [jump] functions, described later.

    @param [stop_on] defines a set of predicates that will be checked
    on each step to decide whether it should stop here and call a
    user-provided [hit] function, or it should continue. The descision
    is made acording to the rule: [if exists stop_on then stop], i.e.,
    it there exists such predicate in a set of predicates, that
    evaluates to true, then stop the disassembly and pass the control
    to the user function [hit].  A few notes: only valid instructions
    can match predicates, and if the set is empty, then it always
    evaluates to false.

    @param [init] initial value of user data, that can be passed
    through handlers (cf., [fold])

    @param [return] a function that lifts user data type ['s] to type
    ['r]. It is useful when you need to perform disassembly in some
    monad, like [Or_error], or [Lwt]. Otherwise, just use [ident]
    function and assume that ['s == 'r].

    In a process of disassembly three user provided callbacks are
    invoked by the engine. To each callback at least two parameters
    are passed: [state] and [user_data]. [user_data] is arbitrary data
    of type ['s] with which the folding over the memory is actually
    performed. [state] incapsulates the current state of the
    disassembler, and provides continuation functions, namely [stop],
    [next] and [back], that drives the process of
    disassembly. This functions are used to pass control back to the
    disassembler.

    [stopped state user_data] is called when there is no more data to
    disassemble. This handler is optional and defaults to [stop].

    [invalid state user_data] is an optional handler that is called on
    each invalid instruction (i.e., a portion of data that is not a
    valid instruction), it defaults to [step], i.e., to skipping.

    [hit state mem insn data] is called when one of the predicates
    specifed by a user was hit. [insn] is actually the instruction
    that satisfies the predicate. [mem] is a memory region spanned by
    the instruction. [data] is a user data. [insn] can be queried for
    assembly string and kinds even if the corresponding modes are
    disabled.  *)
val run :
  ?stop_on:pred list ->
  ?invalid:(('a,'k,'s,'r) state -> mem -> 's -> 'r) ->
  ?stopped:(('a,'k,'s,'r) state -> 's -> 'r) ->
  ?hit:(('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r) ->
  ('a,'k) t ->
  return:('s -> 'r) ->
  init:'s -> mem -> 'r

(** [insn_of_mem dis mem] performes a disassembly of one instruction
    from the given memory region [mem]. Returns a tuple
    [imem,insn,`left over] where [imem] stands for a piece of memory
    consumed in a process of disassembly, [insn] can be [Some ins] if
    disassembly was successful, and [None] otherwise. [`left over]
    complements [imem] to original [mem]. *)
val insn_of_mem : (_,_) t -> mem ->
  (mem * (asm,kinds) insn option * [`left of mem]) Or_error.t

(** current position of the disassembler  *)
val addr : (_,_,_,_) state -> addr

(** current set of predicates *)
val preds : (_,_,_,_) state -> pred list

(** updates the set of predicates, that rules the stop condition.  *)
val with_preds : ('a,'k,'s,'r) state -> pred list -> ('a,'k,'s,'r) state

(** a queue of instructions disassembled in this step  *)
val insns : ('a,'k,_,_) state -> ('a,'k) insns

(** [last s n] returns last [n] instructions disassembled in this
    step. If there are less then [n] instructions, then returns a
    smaller list *)
val last : ('a,'k,'s,'r) state -> int -> ('a,'k) insns

(** the memory region we're currently working on *)
val memory : (_,_,_,_) state -> mem

(** stop the disassembly and return the provided value.  *)
val stop : (_,_,'s,'r) state -> 's -> 'r

(** continue disassembling from the current point. You can change a
    a set of predicates, before stepping next. If you want to continue
    from a different address, use [jump] *)
val step : (_,_,'s,'r) state -> 's -> 'r

(** jump to the specified memory and continue disassembly in it.

    For example, if you want to jump to a specified address, and
    you're working in a [Or_error] monad, then you can:

    [view ~from:addr (mem state) >>= fun mem -> jump mem data]
*)
val jump : (_,_,'s,'r) state -> mem -> 's -> 'r

(** restarts last step.   *)
val back : (_,_,'s,'r) state -> 's -> 'r

(** Operand *)
module Op : sig
  (** operand *)
  type t =
    | Reg of reg
    | Imm of imm
    | Fmm of fmm
  with bin_io, compare, sexp
  include Regular with type t := t
end

type op = Op.t with bin_io, compare, sexp

(** Instruction  *)
module Insn : sig
  type ('a,'k) t = ('a,'k) insn
  val sexp_of_t : ('a,'k) t -> Sexp.t
  val compare : ('a,'k) t -> ('a,'k) t -> int
  val code : ('a,'k) t -> int
  val name : ('a,'k) t -> string
  val kinds : ('a,kinds) t -> kind list
  val is : ('a,kinds) t -> kind -> bool
  val asm : (asm,'k) t -> string
  val ops  : ('a,'k) t -> op array
end

(** Register.  *)
module Reg : sig
  type t = reg
  val code : t -> int
  val name : t -> string
  include Regular with type t := t
end


(** Integer immediate operand  *)
module Imm : sig
  type t = imm
  val to_word  : t -> width:int -> word option
  val to_int64 : t -> int64
  val to_int   : t -> int option
  include Regular with type t := t
end

(** Floating point immediate operand  *)
module Fmm : sig
  type t = fmm
  val to_float : t -> float
  include Regular with type t := t
end
