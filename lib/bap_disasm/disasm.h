#ifndef BAP_DISASM_H
#define BAP_DISASM_H

/** Disassembler.
 *
 *  This module defines a primitive low-level interface to a disassembler.
 *  This interface was created with a performance considerations in mind.
 *
 *  The disassembler can be viewed as a primitive state machine, with
 *  the following logic.
 *
 *  1. perform a disassembly step;
 *  2. push disassembled instruction into queue.
 *  3. move pc to the next instruction
 *  4. if exists predicate p such that p(insn)
 *     or pc >= base + len
 *     then stop
 *     else goto 1.
 *
 *  If disassembly fails in some manner, then disassembler pushes
 *  invalid instruction.
 *
 *  If the set of predicates is empty, then it evaluates to false.  In
 *  other words with an empty set of predicates, the disassembler will
 *  run until it hits the end-of-data condition. This allows to implement
 *  a sweeping mode.
 *
 *  A special is_true predicate allows to implement a single step mode, when
 *  disassembler stops after each iteration. It is guaranteed by implementation,
 *  that if [is_true] is in a set of predicates, then no other predicates will
 *  be evaluated, unless [store_predicates] option is turned on.
 *
 *  Disassembler state consists of:
 *
 *  1. instructions queue
 *  2. predicates set
 *  3. offset
 *
 *
 *  There is also some static state, that doesn't change, but worths
 *  to be mentioned:
 *
 *  1. instructions names table
 *  2. registers names table
 *  3. memory region
 *
 *  Instructions and Registers tables are never changed throught the
 *  life of a disassembler (they are indeed a function of the constructor
 *  parameters). Memory region can be changed after the disassembler was
 *  created, but since it is not changed in the disassembler step it can
 *  be considered static.
 *
 *  Notes:
 *
 *  Every run of dissassembler will increase the size of instructions
 *  queue. Even if there is no data, disassembler will push an invalid
 *  instruction into the queue and stop.
 *
 *  Due to the performance reasons error handling is very deficient,
 *  if any. The calling side should check that preconditions holds,
 *  before the call. If preconditions don't hold the result is
 *  undefined.
 *
 *  One of the precondition that hold for all function, is that argument
 *  of type bap_disasm_type  should be valid.
 */


/** disassembler descriptor  */
typedef int bap_disasm_type;

typedef enum {
    bap_disasm_unknown_error = -1,
    bap_disasm_no_such_backend = -2,
    bap_disasm_unsupported_target = -3,
} bap_disasm_error;


/* In a normal mode the set of predicates defines the stopping condition
 *
 * In a sweep mode, disassembler will not check for predicates and
 * will run until the end of data condition is met.
 *
 * In a step mode disassembler will not check for predicates and will
 * stop after each step
 */

typedef enum {
    bap_disasm_op_reg  = 0,
    bap_disasm_op_imm  = 1,
    bap_disasm_op_fmm  = 2,
    bap_disasm_op_insn = 3 ,
} bap_disasm_op_type;

/** predicates on the instructions  */
typedef enum bap_disasm_insn_p {
    is_true,                    /* true for all instructions */
    is_invalid,                 /* the instruction is invalid */
    is_return,                  /* all returns  */
    is_call,                    /* all calls */
    is_barrier,                 /* control flow won't hit the next insn */
    is_terminator,              /* is basic block terminator */
    is_branch,                  /* branching instructions */
    is_indirect_branch,
    is_conditional_branch,
    is_unconditional_branch,
    may_affect_control_flow
} bap_disasm_insn_p_type;

/* bap_disasm_create(triple,cpu) creates a disassembler for a given
 * triple and cpu.
 * This function is not thread safe.
 * @pre No preconditions. In case of error the corresponding code is returned. */
bap_disasm_type bap_disasm_create(
    const char *backend,
    const char *triple,
    const char *cpu,
    int debug_level);

/* deletes disassembler \a disasm.
 * This function is not thread safe.
 * @pre disasm is valid descriptor */
void bap_disasm_delete(bap_disasm_type disasm);

/** assosiates memory region with a given disassembler. Current offset
 * is automatically reset to 0.
 *
 * @pre memory region between (data+off) and (data+off+len-1)
 * inclusive is readable by a process.
 *
 * @pre off >= 0 && len >= 0. (sic, it can be empty)
 * @pos off = 0
 * */
void bap_disasm_set_memory(
    bap_disasm_type disasm,
    int64_t base,
    const char *data,
    int off,
    int len);

/* by default only the last disassembled instruction can be queried by
 * different predicates. If this option is enabled, then for each
 * disassembled instruction a set of predicates that evaluates to true
 * will be stored, so that it can be later queried. Enabling this
 * option doesn't affect any other options, also current set of
 * predicates remains unchanged.
 * @pre enable => insns queue is empty
 */
void bap_disasm_store_predicates(bap_disasm_type disasm, int enable);


/* by default assembler strings are not stored anywhere and not even
 * created, but with this option enabled, after each diassembly step,
 * the assembly string will be stored for later retrieval. Regardless
 * of the value of this option, you can always retrieve an assembly
 * string for the last disassembled instruction, given it is valid.
 * @pre enable => insns queue is empty.
 */
void bap_disasm_store_asm_strings(bap_disasm_type disasm, int enable);


/* returns a pointer to an instruction name table.
 * The table is created with a disassembler and never changes afterwards.
* It contains a set of null-terminated strings
*/
const char *bap_disasm_insn_table_ptr(bap_disasm_type disasm);

int bap_disasm_insn_table_size(bap_disasm_type disasm);

/* returns a pointer to an register name table.
 * as well as insn table, this one is allocated at the time of
 * disassembler construction and never changes.
 */
const char *bap_disasm_reg_table_ptr(bap_disasm_type disasm);

/* returns the size in bytes of the registers names table */
int bap_disasm_reg_table_size(bap_disasm_type disasm);


/** Operations over a set of predicates  */

/* clears the set of predicates  */
void bap_disasm_predicates_clear(bap_disasm_type disasm);


/* bap_disasm_predicates_push(disasm,p) adds predicate p to the set of
 * predicates.
 * @pre is_supported p
 */
void bap_disasm_predicates_push(bap_disasm_type disasm,
                                bap_disasm_insn_p_type p);

/* returns true, if the predicate is supported by the backend.
 * @pre none.
 */
int bap_disasm_predicate_is_supported(bap_disasm_type disasm,
                                      bap_disasm_insn_p_type p);

/* points a disassembler into specified part of the memory.
 * @pre none.
 * */
void bap_disasm_set_offset(bap_disasm_type disasm, int offset);


/* in a normal mode of operation runs disassembler until one of the
 * predicates evaluates to true or until there is no more data. An
 * empty set of predicates, always evaluates to false.
 *
 * In a sweep mode, runs until the end of the data.
 *
 * In as single step mode, stops after the first atempt.
 *
 * @pre none.
 * @post insn_size is increased by one.
 */
void bap_disasm_run(bap_disasm_type disasm);

/* clears instruction queue and all assosiated data
 * @pre none
 * @post instruction queue is empty
 */
void bap_disasm_insns_clear(bap_disasm_type disasm);

/* returns the amount of instructions in the decompiler queue.  */
int bap_disasm_insns_size(bap_disasm_type disasm);


/* Quering instructions
 *
 * All functions below (if not stated otherwise) share the following
 * precondition:
 *
 *    insn >= 0 && insn < insns_size
 *    || insn was returned by insn_op_insn_value function
 *       (aka subinstruction).
 */

/** returns size in bytes of the insn @n  */
int bap_disasm_insn_size(bap_disasm_type disasm, int insn);


/** returns insn offset in an insn name table */
int bap_disasm_insn_name(bap_disasm_type disasm, int insn);

/** returns a unique code of the insn. The code identifies an
 * instruction in the set of instructions for the specified target.*/
int bap_disasm_insn_code(bap_disasm_type disasm, int insn);

/** returns an offset of the instruction in a memory region  */
int bap_disasm_insn_offset(bap_disasm_type disasm, int insn);

/* returns the size of the representation of the instruction \a insn
 * in a target's assembly.
 * @pre insn is not a subinstruction
 * @pre not satisfies(insn, is_invalid)
 * @pre store_asm_strings option is enabled
 *      || insn = insns_size - 1
 */
int bap_disasm_insn_asm_size(bap_disasm_type disasm, int insn);

/* copies assembly representation to the specified
 * destination.
 * @pre all asm_size preconditions;
 * @pre memory region (dst, dst+asm_size-1) is writable by a process.
 */
void bap_disasm_insn_asm_copy(bap_disasm_type disasm, int insn, void *dst);

/* returns non zero if instruction satisfies predicate.
 * @pre not satisfies(insn, is_invalid)
 * @pre is_supported(p)
 * @pre store_predicates option is enabled
 *      || insn = insn_size - 1
 */
int bap_disasm_insn_satisfies(bap_disasm_type disasm,
                              int insn,
                              bap_disasm_insn_p_type p);


/* Quering operands
 *
 * All functions below share precondition that op is less than
 * ops_size and greater than zero
 */

/* returns the amount of operands for a given instruction  */
int bap_disasm_insn_ops_size(bap_disasm_type disasm,
                             int insn);

/* returns the type of the operand  */
bap_disasm_op_type bap_disasm_insn_op_type(bap_disasm_type disasm,
                                           int insn,
                                           int op);

/* returns an offset in a registers name table.
 * @pre op_type = reg
 */
int bap_disasm_insn_op_reg_name(bap_disasm_type disasm,
                                int insn,
                                int op);

/* returns a unique identifier of the register
 * @pre op_type = reg
 */
int bap_disasm_insn_op_reg_code(bap_disasm_type disasm,
                                int insn,
                                int op);

/* returns value of the integer immediate value.
 * @pre op_type = imm
 */
int64_t bap_disasm_insn_op_imm_value(bap_disasm_type disasm,
                                     int insn,
                                     int op);

/* returns value of the floating point immediate value
 * @pre op_type = fmm
 */
double bap_disasm_insn_op_fmm_value(bap_disasm_type disasm,
                                    int insn,
                                    int op);

/* returns a subinstruction of the instruction. It can be accessed
 * the same way as a "toplevel" instructions, and it is guaranteed,
 * that returned value is not in the set of valid instruction numbers for
 * the current queue.
 *
 * @pre insn is not a subinstruction
 */
int bap_disasm_insn_op_insn_value(bap_disasm_type disasm,
                                  int insn,
                                  int op);

#endif  /* BAP_DISASM_H */
