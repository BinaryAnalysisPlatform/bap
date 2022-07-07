(** C language ABI.

    This module provides a common interface for building ABI support
    modules for C language.
*)

open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
open Monads.Std
open Bap_c_type


(** Function formal parameter is represented as a pair of
    an abstraction of data, that is passed via the parameter,
    and a BIL expression, that denotes the parameter.*)
type param = Bap_c_data.t * exp

(** subroutine argument list is split into three parts:
    [return] is the return arguments, that is optional;
    [params] are regular positional parameters, the length
    of the [params] list must equal to the amount of the
    formals in the function prototype;
    [hidden] are hidden parameters, that are inserted by abi
    to pass special arguments, like [this] pointer or a pointer
    to a structural value, for example.

    The api processor, created by this module, will insert arg terms
    into sub in the following way:

    - nth positional argument corresponds to nth arg term (counting
      from 0).
    - the last arg term corresponds to the return argument, if any;
    - all hidden arguments are put between the last positional and the
      return argument.*)
type args = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
}

(** an abi processor.
    Each architecture registers its own abi processor, that is
    responsible for dispatching the processed subroutine between
    architecture specific abi processors.*)
type t = {
  (** [insert_args sub attrs proto] infer a list of arguments that
      should be inserted for a subroutine [sub] annotated with the
      attribute list [attrs] *)
  insert_args : sub term -> attr list -> proto -> args option;

  (** [apply_attrs attrs sub] transform a subroutine based on the
      semantics of the list of attributes, attached to it. See also,
      C.Attr.register

  *)
  apply_attrs : attr list -> sub term -> sub term;
}

(** [create_api_processor size t] packs an api processor. The
    processor will insert arg terms into each recognized subroutine,
    propagate some known C attributes into corresponding BIR
    attributes, annotate each inserted arg term with its corresponding
    C type and datum model, and annotate each regognized subroutine
    with its C prototype.

    The api processor relies on an availability of a front end parser
    for C language.*)
val create_api_processor : #Bap_c_size.base -> t -> Bap_api.t


(** [data size t] creates an abstraction of data that is represented
    by type [t]. The [size] parameter defines a data model, e.g.,
    sizes of primitive types, padding and alignment restrictions, etc.

    The abstraction includes inner and trailing paddings, when
    necessary. *)
val data : #Bap_c_size.base -> Bap_c_type.t -> Bap_c_data.t


(** [layout size t] computes the c data type layout.

    @since 2.5.0 *)
val layout : #Bap_c_size.base -> Bap_c_type.t -> Bap_c_data.layout


(** [model target] returns the data model for the given target.

    @since 2.5.0  *)
val model : Theory.Target.t -> Bap_c_size.base


(** [apply processor attrs proto sub] applies the abi processor to the
    subroutine [sub].

    The function inserts arguments and attaches appropriate arguments
    to the function and its subterms, such as strores the type of each
    argument, the provided C attributes, stores the prototype, computes
    and attaches data layouts, etc.

    @since 2.5.0 *)
val apply : t -> #Bap_c_size.base ->  attr list -> proto -> sub term -> sub term


(** [arg_intent t] infers argument intention based on its C type.  If
    an argument is passed by value, i.e., it is a c basic type, then
    it is an input argument. If an argument is a reference, but not a
    function, then it is input/output if any value, referenced by the
    argument is non-const. A reference to function always has the
    input intent. If an argyment is a structure or union, then it is
    input/output if any of its fields is input/output.
*)
val arg_intent : Bap_c_type.t -> intent

(** [register name t] registers an abi processor [t] named [name] that
    may be used by subroutines in this project.

    @after 2.5.0 fails if there is already a processor for the given [name].
    @after 2.5.0 the abi name should be a valid target name.
*)
val register : string -> t -> unit
[@@deprecated "[since 2022-07] use the Arg module"]

(** [get_processor name] is used to access an abi processor with its
    name.*)
val get_processor : string -> t option
[@@deprecated "[since 2022-07] use [lookup]"]


(** [lookup t] the abi processor associated with the target [t].

    @since 2.5.0
*)
val lookup : Theory.Target.t -> t option


(** An abstraction of a stack, commonly used in C compilers.   *)
module Stack : sig
  (** [stack = create ?growsup arch] is a function that returns
      [n]'th stack slot *)
  val create : ?growsup:Bool.t -> arch -> int -> exp
end


(** A monadic eDSL for argument passing semantics specification.

    @since 2.5.0 see also the [Language] module for a higher-level
    eDSL, built on top of the primitives described below.

    This DSL helps in defining the abi processor's [insert_args]
    function. The DSL describes the semantics of argument passing that
    is then reified to the [args] structure. The [DSL] is a choice
    monad that enables describing the argument passing grammar using
    backtracking when the chosen strategy doesn't fit. The [reject ()]
    operator will reject the current computation up until the nearest
    choice prompt, e.g., in the following example, computations [e1],
    [e2], and [e3] are rejected and any side-effects that they might
    had are ignored and, instead the [option2] computation is tried
    as if the previous sequence had never happend.

    {[
      choice [
        sequence [e1; e2; e3; reject ()];
        option2;
      ]
    ]}

    Since the purpose of this DSL is to describe how the passed
    arguments are read in terms of BIL expressions, the generated
    specification could be seen as a grammar and the DSL itself as
    a parser combinator, specialized for describing ABI.

    {2 Example}

    Below we define the semantics of [riscv32] and [riscv64] targets.
    Both targets have fully specified register files with properly
    assigned roles and the register order matches with register names
    ordering, so we can use the simplified Arena creating functions.
    We have four independent arenas, two for passing in and out integer
    arguments, and two corresponding arenas for floating-point
    arguments.

    We start with defining the integer calling convention by first
    determining how many register are needed to pass an argument.
    If the size of the argument couldn't be determined we reject the
    computation. Otherwise, if it fits into one register we try to
    pass it via a register fallback to memory if there are no
    registers available. If it requires two registers we first try to
    pass it as an aligned register pair (with the first part going
    through the nearest available even register). If we don't have
    enough aligned registers, we then split it in two parts and pass
    the first part in a register and the second part in the memory.
    Finally, if the size is greater than two words we pass it as an
    implicit reference.

    The floating-point calling convention assumes the presence of the
    hardware floating-point registers but the specification is general
    enough to handle the soft floats convention, as any attempt to
    pass an argument via the hardware floating-point registers will be
    rejected since the corresponding arena will be empty.

    The convention tries to pass a floating-point argument via the
    corresponding register file if it fits into a register otherwise
    it falls back to the integer registers. When an argument fits into
    the floating-point register we first try passing it through the
    floating-point file and if it is out of registers we use available
    integer registers (in riscv with hardware floating-point registers
    it is possible to pass 16 floating-point arguments all in
    registers) and finally use the last resort option of using the memory.

    {[
      module Arg = C.Abi.Arg
      open Arg.Let
      open Arg.Syntax

      let is_floating = function
        | `Basic {C.Type.Spec.t=#C.Type.real} -> true
        | _ -> false

      let data_model t =
        let bits = Theory.Target.bits t in
        new C.Size.base (if bits = 32 then `ILP32 else `LP64)

      let define t =
        let model = data_model t in
        C.Abi.define t model @@ fun _ {C.Type.Proto.return=r; args} ->
        let* iargs = Arg.Arena.iargs t in
        let* irets = Arg.Arena.irets t in
        let* fargs = Arg.Arena.fargs t in
        let* frets = Arg.Arena.frets t in

        (* integer calling convention *)
        let integer regs t =
          Arg.count regs t >>= function
          | None -> Arg.reject ()
          | Some 1 -> Arg.choice [
              Arg.register regs t;
              Arg.memory t;
            ]
          | Some 2 -> Arg.choice [
              Arg.sequence [
                Arg.align_even regs;
                Arg.registers ~limit:2 regs t;
              ];
              Arg.split_with_memory regs t;
              Arg.memory t;
            ]
          | Some _ -> Arg.reference regs t in

        (* floating-point calling convention *)
        let float iregs fregs t =
          Arg.count fregs t >>= function
          | Some 1 -> Arg.choice [
              Arg.register fregs t;
              Arg.register iregs t;
              Arg.memory t;
            ]
          | _ -> integer iregs t in

        let arg iregs fregs r =
          if is_floating r
          then float iregs fregs r
          else integer iregs r in

        Arg.define ?return:(match r with
            | `Void -> None
            | r -> Some (arg irets frets r))
          (Arg.List.iter args ~f:(fun (_,t) ->
               arg iargs fargs t));

        let () = List.iter ~f:define Bap_risv_target.[riscv32; riscv64]
    ]}

    @since 2.3.0
*)
module Arg : sig
  type 'a t

  (** an ordered expendable collection of registers *)
  type arena

  type semantics

  type ctype = Bap_c_type.t


  (** [define ?return args] the toplevel function for defining
      argument passing semantics.

      The function has two entries, the optional [return] entry
      describes the semantics of passing of the return value,
      and the second section describes the semantics of passing the
      list of arguments.

      The semantics is defined as a sequence of these two rules,
      with the return rule evaluated first. Therefore, if [return]
      is rejected the whole semantics will be rejected.
  *)
  val define : ?return:unit t -> unit t -> semantics t

  (** [register arena t] passes the argument of type [t] using
      the next available register in [arena].

      The computation is rejected if no registers are available;
      if [t] doesn't fit into a register in [arena]; or if size
      of [t] can't be determined.
  *)
  val register : arena -> ctype -> unit t


  (** [registers arena t] passes the argument in consecutive
      registers from [arena].

      Rejects the computation if [arena] doesn't have the necessary
      number of registers; the number of required registers is greater
      than [limit]; or if the size of [t] is unknown.

      If [rev] is true, then the allocated registers will be used in
      the reversed order.

      @since 2.5.0 accepts the optional [rev] parameter.
  *)
  val registers : ?rev:bool -> ?limit:int -> arena -> ctype -> unit t


  (** [align_even arena] ensures that the first available register in
      [arena] has even number.

      Registers in an arena are enumerated from zero in the order of
      their appearence in the arena specification. This function
      removes, when necessary, a register form the arena, so that the
      next available register has an even number.

      The computation is rejected if there are no more even registers
      in [arena].
  *)
  val align_even : arena -> unit t

  (** [deplet arena] unconditionally consumes all registers in arena.

      The computation is never rejected.
  *)
  val deplet : arena -> unit t


  (** [discard arena] discards one location from [arena].

      @param n if specified discards [n] registers instead of one.

      The computation is never rejected, if the arena is empty nothing
      is changed.

      @since 2.5.0  *)
  val discard : ?n:int -> arena -> unit t

  (** [reference arena t] passes a hidden pointer to [t] via
      the first available register in [arena].

      Rejects the computation if there are no available registers in
      [arena]. The size of [t] is not required.

      Note, that [reference] and [hidden] are increasing the number of
      hidden arguments of a subroutine, but do not add the actual
      arguments.  *)
  val reference : arena -> ctype -> unit t


  (** [pointer arena t] passes argument [t] as a pointer.

      Rejects the computation if [arena] is empty. The size of [t] is
      not required.

      @since 2.5.0  *)
  val pointer : arena -> ctype -> unit t


  (** [hidden t] inserts a hidden pointer to [t] into the next
      available stack slot.

      The computation is rejected if the target doesn't have a stack.

      @since 2.5.0  *)
  val hidden : ctype -> unit t

  (** [memory t] passes the argument of type [t] in the next
      available stack slot.

      Rejects the computation if the size of [t] is not known or
      if the target doesn't have a register with the stack pointer
      role.

      The address of the slot is aligned corresponding to the
      alignment requirements of [t] but no less than the
      minimal data alignment requirements of the architecture or
      the natural alignment of the stack pointer.

      Note, passing a number arguments via a descending stack using
      [memory] will pass the arguments in the right-to-left (RTL aka
      C) order, i.e., the first passed argument will end up at the
      bottom (will have the minimal address). Use [push] if you want
      the left-to-right order.

  *)
  val memory : ctype -> unit t

  (** [rebase off] rebases the stack position by [n] words.

      @since 2.5.0
  *)
  val rebase : int -> unit t

  (** [split a1 a2 t] passes the lower half of the value
      via registers in the arena [a1] and the higher via the registers
      in the arena [a2].

      The compuation is rejected if either arean doesn't have enough
      registers to pass its half, or if the size of [t] is odd or unknown.

      @since 2.5.0 *)
  val split : arena -> arena -> ctype -> unit t


  (** [split_with_memory arena t] passes the low order part of the
      value in a registers (if available) and the rest in the memory.

      The size of the part that is passed via the registers is equal
      to the number of avaliable registers, but not greater than the
      [limit] (if specified). The part that is passed via the
      stack is aligned to the stack boundary.

      If [rev] is [true] then pass the object in the reversed order.

      Rejects the computation if the size of [t] is not known; if
      [arena] is empty; or if some other argument is already passed
      via memory.

      @after 2.5.0 accepts the [rev] parameter.
      @after 2.5.0 accepts the [limit] parameter.

      @after 2.5.0 passes as much as possible (up to the limit) of the
      object via registers.

      @before 2.5.0 was passing at most one word via registers.
  *)
  val split_with_memory : ?rev:bool -> ?limit:int -> arena -> ctype -> unit t


  (** [push t] pushes the argument of type [t] via stack.

      Rejects the computation if the size of [t] is not known.

      The address of the slot is aligned corresponding to the
      alignment requirements of [t] but no less than the
      minimal data alignment requirements of the architecture or
      the natural alignment of the stack pointer.

      When passing a number of arguments via a descending stack, the
      last pushed argument will be at the bottom of the stack, i.e.,
      will have the minimal address. This corresponds to the LTR aka
      Pascal ordering.
  *)
  val push : ctype -> unit t

  (** [count arena t] counts the number of registers need to pass a
      value of type [t].

      Returns [None] if the size of [t] is not known or if the [arena]
      size is empty.
  *)
  val count : arena -> ctype -> int option t


  (** [size t] is the size in bits of an object of type [t].

      The computation is rejected if the size is unknown, i.e., the
      type is incomplete.

      @since 2.5.0 *)
  val size : ctype -> int t

  (** [require cnd] rejects the computation if [cnd] doesn't hold.

      @since 2.5.0  *)
  val require : bool -> unit t

  (** [either option1 option2] tries to pass using [option1] and
      if it is rejected uses [option2].

      For example, [either (register x) (memory x)] tries to pass
      [x] via a register and if it is not possible (either because
      [x] doesn't fit into a register or there are no registers
      available) tries to pass it via memory.
  *)
  val either : 'a t -> 'a t -> 'a t


  (** [choice [o1 o2 ... oN]] tries options in order until the first
      one that is not rejected.  *)
  val choice : 'a t list -> 'a t

  (**  A high-level ABI-specification language.

       This module makes it easier to describe various calling
       conventions using high-level combinators, built on top
       of the lower level primitives of the [Arg] language.

       The idea is that you can open [Arg.Language] and have all
       combinators available in the scope. To enable language
       extension, we also provide the versioned modules,
       [Arg.Language.V1], and so on. Every extension of the language,
       i.e., an addition of a new operator or combinator, will
       go into a separate module, so that if you are using
       [Arg.Language.Vx] it is guaranteed that the language changes
       will not break anything.


       In [Arg.Language] we describe calling conventions declaratively
       using a list of commands, where each command is a guarded
       statement, i.e., a pair of a predicate and the statement. The
       statement is using the [Arg] operators to describe argument
       passing routine, and the predicate checks if this is applicable
       to the given arguement. Various combinators combine commands
       and predicates, into a final statement that fully describes the
       argument passing procedure for the given subroutine.

       @since 2.5.0
  *)
  module Language : sig

    type predicate = ctype -> bool
    type statement = ctype -> unit t
    type predicates = predicate list
    type statements = statement list
    type command = predicate * statement
    type commands = command list
    type 'a cls = [>] as 'a

    module type V1 = sig

      (** [install target data_model specification]

          The toplevel function that is used to install the calling
          convention for the given target.  The general syntax follows
          this structure,

          {[
            describe t data @@ fun declare ->
            let* arena1 = <arena-specification> in
            let* arenaN = <arena-specification> in
            let return ~alignment x = <statement> in
            let arg ~alignment x = <statement> in
            let finish = <finalization-procedure> in
            let* () = <initialization-procedure> in
            declare ~finish ~return arg
          ]}

          For example, the following specification describes ARM
          calling convention AAPCS32,

          {[
            let define t =
              install t model @@ fun describe ->
              let* iargs = Arg.Arena.iargs t in
              let* irets = Arg.Arena.irets t in
              let rev = Theory.Endianness.(Theory.Target.endianness t = le) in
              let return ~alignment:_ size = select [
                  C.Type.is_basic, select [
                    is (size <= 32 * 2), Arg.registers ~rev  irets;
                    otherwise, Arg.reference iargs;
                  ];
                  is (size <= 32), Arg.register irets;
                  otherwise, Arg.reference iargs;
                ] in
              describe ~return @@ fun ~alignment _ ->
              sequence [
                is (alignment = 64), const (Arg.align_even iargs);
                always, choose [
                  Arg.split_with_memory ~rev iargs;
                  Arg.memory
                ];
              ]
          ]}
      *)
      val install : Theory.Target.t -> #Bap_c_size.base ->
        ((?finish:(unit t) ->
          return:(alignment:int -> int -> statement) ->
          (alignment:int -> int -> statement) -> unit t) ->
         unit t) ->
        unit


      (** [sequence cmd] executes a sequence of predicated statements.

          Executes all commands in the specified order. A command's
          statement is executed if the commands predicate is evaluates
          to [true]. Otherwise the statement is skipped.

          If an evaluated statement rejects a computation then the
          whole sequence will be rejected. *)
      val sequence : commands -> statement

      (** [select t spec] selects the first applicable option to pass [t].

          The form,
          {[
            select [
              pred1, option1;
              pred2, option2;
              ...,...;
              predN, optionN;
            ]
          ]}

          Tries [pred1 arg], [pred2 arg], ..., [predN arg] in order until
          the first one that returns [true] and uses the corresponding
          option to pass the argument.

          The computation is rejected if either the selected option
          rejects the computation or none options were selected. *)
      val select : commands -> statement

      (** [case classifer commands] case analysis.

          The case combinator first classfies the argument using the
          [classfier] function, which shall return a polymoprhic
          variant, and then selects a command that matches the
          selected class.

          Rejects the computation if there's no matching class.
      *)
      val case : (ctype -> 'a cls t) -> ('a cls * statement) list -> statement

      (** [any ps] holds if any of [ps] holds. *)
      val any : predicates -> predicate

      (** [all ps] holds if all of [ps] hold.*)
      val all : predicates -> predicate

      (** [neither ps] holds if neither of [ps] hold. *)
      val neither : predicates -> predicate


      (** [is cnd] holds if [cnd] holds.

          Example, {[[
            is (size > 64), memory;
            otherwise, registers;

          ]]}
      *)
      val is : bool -> predicate

      (** [otherwise] is a predicate that is always [true].

          I.e., it is [is true].

          This predicate is supposed to be used with the [select]
          combinator as the last, catch-all, predicate, e.g.,

          {[
            select arg [
              is_fundamental, pass_fundamental;
              is_floating, pass_floating;
              otherwise, pass_memory;
            ]
          ]} *)
      val otherwise : predicate

      (** [always] is a predicate that is always [true], i.e., [is true].  *)
      val always : predicate

      (** [never] is a predicate that never holds, i.e., [is false].  *)
      val never : predicate

      (** [choose options] tries options in order until the first on that
          is not rejected.

          This combinator is like [choice] but is supposed to be used
          inside [select], e.g.,

          {[
            select arg [
              p1, choose [o1, o2];
              either [p2; p3], choose [o3,o4];
            ]
          ]} *)
      val choose : statements -> statement

      (** [combine xs] combines statements into a signle statement.

          Evaluates statements in the order they are specified. If any
          of the statements rejects then the whole statement will be
          rejected. *)
      val combine : statements -> statement

      include Monad.Syntax.S with type 'a t := 'a t
      include Monad.Syntax.Let.S with type 'a t := 'a t

    end
    include V1
  end


  (** [reify t size args] compiles the argument passing specification.

      If the spec is not rejected the returned structure will contain
      the reification of the argument passing semantics.

  *)
  val reify : Theory.Target.t -> #Bap_c_size.base -> semantics t -> args option



  include Monad.S with type 'a t := 'a t
  include Monad.Choice.S with type 'a t := 'a t


  (** An ordered collection of locations.

      Arena is an expendable collection of data locations (usually
      registers) that are used to pass arguments. Passing an argument
      via an arena location consumes it so it is no longer available in the
      same computation.

      If a computation that used an arena location is later rejected then the
      register is available again (the same as with any other
      side-effects of a rejected compuation).

      The order of locations in arena, as well as their numbering
      according to that order, usually matters. Many targets have
      registers with the alphabetic orders of registers matching their
      arena orders (with notable exception of x86) that enables the
      direct usage of the [Theory.Target.regs] function to create
      arenas.  *)
  module Arena : sig

    (** [create regs] creates an arena from the ordered list of registers.

        All registers must have the same size and the list could be
        empty. The registers will be used in the order of their
        appereance in the [regs] list. *)
    val create : _ Theory.Var.t list -> arena t

    (** [of_exps xs] creates an arena from the ordered list of expressions.

        The expressions must have the same type and the list could be
        empty. The locations will be used in the order in which they
        were specified.

        @since 2.5.0 *)
    val of_exps : exp list -> arena t

    (** [of_roles t roles] creates an arena from registers of the
        specified roles.

        The registers are ordered in the alphabetic order. The
        returned arena might be empty. *)
    val of_roles : Theory.role list -> Theory.Target.t -> arena t


    (** [iargs t] the integer argument arena.

        An alias to [of_roles [function_argument; integer]]
    *)
    val iargs : Theory.Target.t -> arena t


    (** [irets t] the integer return values arena.

        An alias to [of_roles [function_return; integer]]
    *)
    val irets : Theory.Target.t -> arena t

    (** [fargs t] the floating-point argument arena.

        An alias to [of_roles [function_argument; floating]]
    *)
    val fargs : Theory.Target.t -> arena t


    (** [frets t] the floating-point return values arena.

        An alias to [of_roles [function_return; floatin]]
    *)
    val frets : Theory.Target.t -> arena t
  end
end


(** [define target pass] the high-level ABI specification function.

    The function creates an abi processor and registers it using the
    name obtained from the [target]. The [pass] function is used to
    define the [insert_args] method (with the [sub] argument
    ignored).

    The function also registers an ABI pass that checks the project
    target and if it matches with the passed [target] the function
    creates and registers the C API processor.
*)
val define : Theory.Target.t -> #Bap_c_size.base ->
  (attr list -> proto -> Arg.semantics Arg.t) -> unit
