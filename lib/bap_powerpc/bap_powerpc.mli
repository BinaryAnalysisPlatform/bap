
(**

    {2 Intro}

    This module is the only one that needed to write lifter functions.
    The main idea is to make a life of lifter writers as easy as
    possible. This implies that only a few lines of code should be
    enough to describe any instruction.

    We introduce RTL - the language we expect to be very expressive,
    with lots of details hidden under the hood, so a user should not
    care about minor details.

    So proposed usage is just to open at the very beginning of your
    module:

    {[
      open Bap_powerpc.Std
    ]}

    {2 RTL}

    The central part of this module is RTL. It contains expressions,
    operations over expressions and statements.
    Basically, any line that ended with ';' is a statement,
    and any part of it is either expression(s), or operation over
    expression(s).

    {3 Bitwidth and Signedness}

    Any expression in RTL has a notion of signedness and bitwidth.

    If an operation is applied to expressions with different signs,
    any unsigned expression is implicitly casted to signed one.

    If an operation is applied to expressions with different bitwidths,
    an expression with the smaller bitwidth is implicitly casted
    (extended) to the bitwidth of another expression.

    {3 Expressions}

    There are only few ways to construct an expression:
     - from instruction operand
     - from constant
     - from string
     - by defining temporary variable

    To construct an expression that denotes a register and treat
    its content as an unsigned value, one should write:

    {v
     let ra = unsigned cpu.reg op.(0)
              -------- ------- ------
                 ^         ^      ^
                 |         |      |
      content is |         |      |
      unsigned __|         |      |
                           |      |
                 claim register  from operands array at index 0
    v}

    Immediate instructions operands are constructed in the same
    way:

    {[
      let im = signed imm op.(1)
    ]}

    Also one may create variables for convenience:

    {[
      let x = unsigned var halfword
    ]}

    that is a creation of variable of bitwidth 16. Other useful
    bitwidthes are bit, byte, word, doubleword, quadroword. And
    also it's possible to create a variable of arbitrary bitwidth:

    {[
      let x = signed var (bitwidth 10)
    ]}

    It's also possible to create an epxression from integer constant:

    {[
      let x = unsigned const word 42
    ]}

    And create an expression from string:

    {[
      let x = unsigned of_string "0xFFFF_FFFF_FFFF"
    ]}

    {4 Taking a part}

    There is a general way to take a part of an expression:
    {extract exp from to}, where a [from] denotes a more significant
    bit than [to].

    Also, there are few more convenient and readable ways to take a
    part, e.g.:
    low word x  - take the last word from [x]
    high byte x - take first (the most significant) byte
    last x 5    - take last (the least significant) bits
    first x 2   - take second bit
    msb x       - take the most significant bit

    Note, that taking a part of a bigger width from expression is
    also possible, see example 1 below.

    Taking a part always results to an unsigned expression. And
    sign bit interpretation depends on further using of a result
    expression. So don't use it for sign casting, just use
    expression as it is where signedness matter.

    Example 1. Taking a part explicitly. The result is 0x0000_FFFF.
    {[
     let x = signed const halfword 0xFFFF in
     let y = signed var word in
     RTL.[
       y := last word x;
     ];
    ]}

    Example 2. Assignment to signed, without taking a part.
    The result is 0xFFFF_FFFF.
    {[
     let x = signed const halfword 0xFFFF in
     let y = signed var word in
     RTL.[
       y := x;
     ]
    ]}

    {4 Concatenation}

    Concatenation always return an unsigned expression. It's still
    possible to use a result as signed expressions, e.g. by assigning
    it to signed expression or use in signed binary/unary operation.

    {4 Operators}

    There are lot's math operators: plus, modulo, less than etc.
    All they take one or more expressions and also return expression:
    x + y, lnot y, x lsl y ...

    {4 Assignment}

    The only operator that takes an expression and returns a statement
    is an assignment. It is a very important and expressive operator
    in RTL. The right-hand side of an assignment is always treated to
    have the same sign and width as a left one:
    {[
      ra := zero;
      rb := rc ^ rd;
    ]}
    Assuming, that zero is just one bit and all ra, rb, rc, rd are 32-bit
    expressions we will get ra, with all bits set to zero, and rb
    equaled to rd, since a concatenation [rc ^ rb] returns a 64 bit
    expression and we may assign only 32 bit.

    An expression in left hand side of assignment is always either of
    expressions:
     - constructed with var/reg constructors
     - expressions from cpu model.
     - taking a part or concatenation of two cases above
    So there are few examples of correct assignment:

    {[
      low byte rt := ra + rb;
      cpu.cr := zero;
      nbit cpu.cr 1 := one;
    }]


    {2 Bit,byte,whatever Order}

    Everywhere in this module, where a notion of bit position
    (byte,word ...) does matter, a numeration starts from
    the most significant bit (byte,word, ...).

    Example 1. [y] will be set to 0xAB, because first byte
    requested:
    {[
     let x = signed const halfword 0xABCD in
     let y = signed var byte in
     RTL.[
       y := first byte x;
     ];
    ]}

    Example 2. [y] will be set to 0xCD, because last byte
    requested:
    {[
     let x = signed const halfword 0xABCD in
     let y = signed var byte in
     RTL.[
       y := last byte x;
     ];
    ]}

    Example 3. [y] will be set to one, because second byte
    requested (numeration starts from zero):
    {[
     let x = unsigned of_string "0b010000" in
     let y = unsigned var bit in
     RTL.[
       y := nth bit x 1;
     ];
    ]}

    {2 Lifter}

    Any lifter function must have two arguments. The first one is
    a model of CPU that contains all information like registers,
    memory access, instruction address, etc. The second argument
    is an array of instruction operands, and it's a user responsibility
    to treat each element of this array according to instruction
    semantics. So, if the first operand of add instruction is a target
    register, the second operand is a source register, and the
    third operand is an immediate:
    {[
      let tar = signed cpu.reg ops.(0) in
      let src = signed cpu.reg ops.(1) in
      let imm = unsigned imm ops.(2) in
      RTL.[
        tar := src + imm;
      ]
    ]}

    {2 Misc}

    There are few useful constructions that either a part of RTL
    (if_, foreach) or simplify code (when_, ifnot, switch).
    Also, one needs to register a lifted function, so
    it could be called when appropriative instruction will be
    encountered. There are two operators for this purpose:
    - (>>) - just registers a function (see example below)
    - (>.) - does the same, plus does some extra job (see
             a description below)

    {2 Comlete example}

    To be more concrete let's create an artificial example.
    {[
      1 let sort_of_add cpu ops =
      2   let rt = unsigned reg ops.(0) in
      3   let ra = signed reg ops.(1) in
      4   let im = unsigned imm ops.(2) in
      5   let rc = unsigned reg ops.(3) in
      6   let tm = signed var doubleword in
      7   let xv = unsigned const word 42 in
      8   let sh = unsinged const byte 2 in
      9   RTL.[
     10        rt := ra + im;
     11        tm = cpu.load rt halfword + xv;
     12        rc := (tm lsl sh) + cpu.ca;
     13    ]
     14   let () =
     15     "SomeSortOfAdd"  >> sort_of_add;
    ]}

    There is a lifter for instruction "SomeSortOfAdd". It's required
    it has two arguments: cpu model and operand array.
    An author read an ISA of Power PC architecture carefully and
    figured out that this instruction has four operands, and that the
    first, the second and the fourth argument are registers and the
    third one is an immediate. And instruction has the following
    semantics.
    An effective address is a sum of the content of [ra] register and
    immediate. An effective address is stored in [rt] register. A
    halfword stored at this address must be summed with 42, shifted
    left twice and summed with carry flag. And the result must be
    written to [rc] register.

    How did author implement lifter for this instruction:
    line  1    : defined a function with two arguments
    lines 2-5  : parsed instruction operands
    lines 6-8  : defined useful constants
    lines 9-13 : wrote RTL code for this instruction.
    lines 14-15   : registered lifter for this instruction

    What happens on each line of RTL code:
    line 10: sum of signed [ra] and unsigned imm is a signed expression,
             because one of the operands is signed. But an unsigned
             result is placed in [rt], since [rt] is unsigned too.
    line 11: load from memory at address from [rt] is summed with 42
             and assigned to variable [tm]. Note, there are two width
             extension under the hood: loaded halfword is extended to
             up to a word bitwidth (since it's a bigger bitwidth among
             sum operand) and than sum extended to a doubleword
             bitwidth with respect to a [tm] sign. So, the result of
             this sum is treated as a signed.
    line 12: Logical shift returns an unsigned result which is summed
             with unsigned value. The interesting part is that it's
             safe to add one-bit value (flag is one bit width) and a
             doubleword.
*)

open Core_kernel.Std
open Bap.Std

module Std : sig

  (** Operands and registers bitwidth.  *)
  type bitwidth

  val bit  : bitwidth
  val byte : bitwidth
  val word : bitwidth
  val halfword : bitwidth
  val doubleword : bitwidth
  val quadword : bitwidth
  val bitwidth : int -> bitwidth

  type exp [@@deriving bin_io, compare, sexp]
  type rtl [@@deriving bin_io, compare, sexp]

  (** expression constructor  *)
  type 'a ec

  (** CPU model  *)
  type cpu = {
    (** [cpu.load address size] *)
    load      : exp -> bitwidth -> exp;

    (** [cpu.store address data size]  *)
    store     : exp -> exp -> bitwidth -> rtl;

    (** [cpu.jmp address]  *)
    jmp       : exp -> rtl;

    (** address of current instruction *)
    cia       : exp;

    (** address size of current arch   *)
    addr_size : bitwidth;

    (** gpr bitwidth for current arch  *)
    gpr_width : bitwidth;

    (** registers  *)
    reg       : (op -> exp) ec; (** reg constructor - constructs a register from operand *)
    gpr       : int -> exp; (** general purpose registers 0..31 *)
    fpr       : int -> exp; (** floating-point registers 0..31  *)
    vr        : int -> exp; (** vector register 0..31           *)
    ctr       : exp;       (** count register      *)
    lr        : exp;       (** link register       *)
    tar       : exp;       (** target register     *)
    cr        : exp;       (** condition register  *)
    cr0       : exp;       (** condition register field 0 *)
    cr1       : exp;       (** condition register field 1 *)
    cr2       : exp;       (** condition register field 2 *)
    cr3       : exp;       (** condition register field 3 *)
    cr4       : exp;       (** condition register field 4 *)
    cr5       : exp;       (** condition register field 5 *)
    cr6       : exp;       (** condition register field 6 *)
    cr7       : exp;       (** condition register field 7 *)

    (** fixed precision flags *)
    so        : exp; (** summary overflow        *)
    ca        : exp; (** carry flag              *)
    ov        : exp; (** overflow flag           *)
    ca32      : exp; (** carry out of 32 bits    *)
    ov32      : exp; (** overflow of 32 bits     *)
  }

  (** The type of lifter functions *)
  type lift = cpu -> op array -> rtl list

  (** [signed ec] - returnst a signed expression from given [ec] *)
  val signed : 'a ec -> 'a

  (** [unsigned ec] - returns an unsigned expression from given [ec] *)
  val unsigned : 'a ec -> 'a

  (** imm constructor - constructs an immediate from operand *)
  val imm : (op -> exp) ec

  (** var constructor - constructs a variable of bitwidth *)
  val var : (bitwidth -> exp) ec

  (** const constructor - constructs a constant of [bitwidth] and integer *)
  val const : (bitwidth -> int -> exp) ec

  (** [of_string] - constructs an expression from string.
      String must be either in a decimal, binary, octal or hexadecimal format.
      Bitwidth of an expression is defined as following:
      if format is decimal then bitwidth = number of significant bits
      else bitwidth = number of all lister bits in a string.
      Examples:
       - bitwidth of [unsigned of_string "0b00"] is eqauls to 2
       - bitwidth of [unsigned of_string "0o474"] is eqauls to 9;
       - bitwidth of [unsigned of_string "0b03FA"] is eqauls to 16;
       - bitwidth of [unsigned of_string "42"] is eqauls to 6; *)
  val of_string : (string -> exp) ec

  (** Set of operators. Briefly it contains next operators:
      - assignment
      - math operators: +, -, *, \, %, <, >, <= , >= , =, <>
      - math signed operators: \$, %$, <$, >$, <=$, >=$
      - logical operators: lsl, lsr, lnot, land, lor, lxor  *)
  module RTL : sig

    (** [x := y] - assignment *)
    val ( := ) : exp -> exp -> rtl

    (** [x + y] - sum *)
    val ( + ) : exp -> exp -> exp

    (** [x - y] - substraction *)
    val ( - ) : exp -> exp -> exp

    (** [x * y] - multiplication *)
    val ( * ) : exp -> exp -> exp

    (** [x / y] - division *)
    val ( / ) : exp -> exp -> exp

    (** [x /$ y] - signed division *)
    val ( /$ ) : exp -> exp -> exp

    (** [x ^ y] - concatenation *)
    val ( ^ ) : exp -> exp -> exp

    (** [x % y] - modulo*)
    val ( % ) : exp -> exp -> exp

    (** [x %$ y] - signed modulo *)
    val ( %$ ) : exp -> exp -> exp

    (** [x < y] - less than*)
    val ( < ) : exp -> exp -> exp

    (** [x > y] - greater than*)
    val ( > ) : exp -> exp -> exp

    (** [x <= y] - less than or equal*)
    val ( <= ) : exp -> exp -> exp

    (** [x >= y] - greater than or equal *)
    val ( >= ) : exp -> exp -> exp

    (** [x = y] - equal *)
    val ( = ) : exp -> exp -> exp

    (** [x <> y] - not equal *)
    val ( <> ) : exp -> exp -> exp

    (** [x <$ y] - signed less than *)
    val ( <$ ) : exp -> exp -> exp

    (** [x >$ y] - signed greater than *)
    val ( >$ ) : exp -> exp -> exp

    (** [x <=$ y] - signed less than or equal *)
    val ( <=$ )  : exp -> exp -> exp

    (** [x >=$ y] - signed greater than or equal *)
    val ( >=$ )  : exp -> exp -> exp

    (** [x lsl y] - logical shift left *)
    val ( lsl )  : exp -> exp -> exp

    (** [x lsr y] - logical shift right *)
    val ( lsr )  : exp -> exp -> exp

    (** [x lor y] - logical or *)
    val ( lor )  : exp -> exp -> exp

    (** [x land y] - logical and *)
    val ( land ) : exp -> exp -> exp

    (** [x lxor y] - lofical xor*)
    val ( lxor ) : exp -> exp -> exp

    (** [lnot x] - logical not*)
    val lnot : exp -> exp

    (** [if_ cond then_ else_] *)
    val if_ : exp -> rtl list -> rtl list -> rtl

    (** [foreach step e rtl] - repeat [rtl] for each [step] of [e].
        One must create an iteration variable to iterate over some
        expression. So, in example below, assuming the first operand
        is a 64-bit register, [cnt] will be equal to 8:
        ...
        let reg = unsigned reg ops.(0) in
        let cnt = unsigned const byte in
        let byte_i = unsigned var byte in
        RTL.[
           cnt := zero;
           foreach byte_i reg [
               cnt := cnt + one;
           ]
        ]
        ...

        One can use iteration variable to change content of register,
        e.g. :
        ...
        RTL.[
           cnt := zero;
           foreach byte_i reg [
               if_ (cnt = zero) [
                   byte_i := zero;
               ]
               cnt := cnt + one;
           ]
        ]
        ...
        will set a most significant byte of [reg] to zero *)
    val foreach : exp -> exp -> rtl list -> rtl

    (** [message m] - embeds a string [m] in code *)
    val message : string -> rtl

  end

  (** [zero] is a one bit length expression set to zero *)
  val zero : exp

  (** [one] is a one bit length expression set to one *)
  val one  : exp

  (** [extract e lx rx] extracts portion of [e] starting
      at bit [lx] and ending at bit [rx], all bounds
      are inclusive. Bits indexes start from the most
      significant bit. *)
  val extract : exp -> int -> int -> exp

  (** [low width e] - extracts low [width] bits from [e]  *)
  val low : bitwidth -> exp -> exp

  (** [high width e] - extracts high [width] bits from [e]  *)
  val high : bitwidth -> exp -> exp

  (** [first e n] - extracts first [n] bits from [e], starting from
      the most significant bit *)
  val first : exp -> int -> exp

  (** [last e n] - extracts last [n] bits from [e], where the
      last bit is the least significant bit *)
  val last : exp -> int -> exp

  (** [nth width e n] - extracts a portion of [e] of width [width] at
      index [n], where each index points to a portion of width [width].
      Indexes are zero based and started from most significant portion.
      E.g. [nth halfword e 1] extracts a second halfword from [e] *)
  val nth : bitwidth -> exp -> int -> exp

  (** [msb e] - extracts the most significant bit from [e] *)
  val msb : exp -> exp

  (** [lsb e] - extracts the least significant bit from [e] *)
  val lsb : exp -> exp

  (** [when_ cond rtl] = if_ cond rtl [] *)
  val when_ : exp -> rtl list -> rtl

  (** [ifnot cond rtl] = if_ cond [] rtl *)
  val ifnot : exp -> rtl list -> rtl

  (** switch clause  *)
  type clause

  (** [switch x clauses] - create a switch construction.
      Example:
      ...
      ra := <...>
      switch (x) [
        case one   [ rs := <...>; ];
        case zero  [ rt := <...>;
                     rs := <...>; ];
        default [rs := zero];
      ]
      ...  *)
  val switch  : exp -> clause list -> rtl

  (** [case exp code] - creates a switch case *)
  val case : exp -> rtl list -> clause

  (** [default code] - creates a switch default *)
  val default : rtl list -> clause

  (** [width e] - returns width of [e] as an expression *)
  val width : exp -> exp

  (** [bil_of_rtl rtl] - returns a bil code *)
  val bil_of_rtl : rtl list -> bil

  (** [concat insn insn'] - returns a lifter, that is a
      concatenation of code for insn and insn' *)
  val concat : lift -> lift -> lift

  (** [^] same as concat  *)
  val (^) : lift -> lift -> lift

  (** Registration *)

  (** [name >> lift]  - registers a lifter for instruction [name]  *)
  val (>>) : string -> lift -> unit

  (** [name >. lift] - registers a lifter for dot version of instruction
      [name], but also extend an RTL code with signed comparison of the
      result to a zero and writing CR0 field according to this
      comparison. It's also assumed that a first instruction operand
      is used for storing of a result. *)
  val (>.) : string -> lift -> unit

  module type Model = sig
    type t
    (** all general purpose registers *)
    val gpr  : t String.Map.t
    val gpri : t Int.Map.t

    (** all floating point registers *)
    val fpr : t String.Map.t
    val fpri : t Int.Map.t

    (** all vector registers *)
    val vr : t String.Map.t
    val vri : t Int.Map.t

    (** count register  *)
    val ctr : t

    (** link register  *)
    val lr : t

    (** target register  *)
    val tar : t

    (** condition register bits, starting from msb *)
    val cri : t Int.Map.t

    (** condition register bits *)
    val crn : t String.Map.t

    (** fixed precision flags *)
    val so : t   (** summary overflow        *)
    val ca : t   (** carry flag              *)
    val ov : t   (** overflow flag           *)
    val ca32 : t (** carry out of 32 bits    *)
    val ov32 : t (** overflow of 32 bits     *)
  end

  module type Model_exp = sig
    include Model with type t := exp
    (** condition register  *)
    val cr : exp

    (** condition register fields *)
    val cr_fields  : exp String.Map.t
    val cri_fields : exp Int.Map.t
  end

  module type PowerPC = sig
    module E : Model_exp
    include Model with type t := var

    val mem : var
    val flags : Var.Set.t
    val gpr_bitwidth : int
    val fpr_bitwidth : int
    val vr_bitwidth  : int
    val cr_bitwidth  : int
    val lr_bitwidth  : int
    val ctr_bitwidth : int
    val tar_bitwidth : int
  end

  module PowerPC_32 : PowerPC
  module PowerPC_64 : PowerPC

  module PowerPC_32_cpu : CPU
  module PowerPC_64_cpu : CPU

  module T32 : Target
  module T64 : Target
  module T64_le : Target

end
