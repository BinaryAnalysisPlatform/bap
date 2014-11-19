#ifndef BAP_DISASM_H
#define BAP_DISASM_H

/** Disassembler.
 *
 *  This module defines a primitive low-level interface to a disassembler.
 *  This interface was created with a performance considerations in mind.
 *  Also, although it is not stated explicitly in the code, it mimicks,
 *  to some extent, the MCDisassembler from LLVM, but it is supposed that
 *  it can be implemented by other disassemblers, like capstone or udism.
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
 *  Disassembler state consists of:
 *
 *  1. instructions queue
 *  2. predicates set
 *  3. offset
 *  4. instruction disasm table
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
 *  If the set of predicates is empty, then it evaluates to false.
 *  In other words with an empty set of predicates, the disassembler
 *  will run until it hits the end-of-data condition.
 *
 *  If given predicate is not supported by the backend, then it will
 *  always evaluate to false. In other words, it won't be added to the
 *  set.
 *
 *  Every run of dissassembler will increase the size of instructions
 *  queue. Even if there is no data, disassembler will push an invalid
 *  instruction into the queue and stop.
 *
 *  Due to the performance reasons error handling is very deficient,
 *  if any.  It is supposed that all checks are made by a caller side,
 *  that should be written in a high-level language. By no means, this
 *  interface was designed for the use from c-code. Anyway, for the
 *  debugging purposes, we provide a facility that allows to dump,
 *  debug information to the stderr.
 *
 *
 */


/** disassembler descriptor  */
typedef int bap_disasm_type;

typedef enum bap_disasm_error {
    BAP_DISASM_UNKNOWN_ERROR = -1,
    BAP_DISASM_NO_SUCH_BACKEND = -2
} bap_disasm_error;

typedef enum bap_disasm_op_type {
    bap_disasm_op_reg,
    bap_disasm_op_imm,
    bap_disasm_op_fmm,
    bap_disasm_op_insn,
} bap_disasm_op_type;

/** predicates on the instructions  */
typedef enum bap_disasm_insn_p {
    is_true,
    is_invalid,
    is_return,
    is_call,
    is_barrier,
    is_terminator,
    is_branch,
    is_indirect_branch,
    is_conditional_branch,
    is_unconditional_branch,
    may_affect_control_flow
} bap_disasm_insn_p_type;

/* bap_disasm_create(triple,cpu) creates a disassembler for a given
 * triple and cpu */
bap_disasm_type bap_disasm_create(
    const char *backend,
    const char *triple,
    const char *cpu,
    int debug_level);

/** assosiates memory region with a given disassembler */
void bap_disasm_set_memory(
    bap_disasm_type disasm,
    int64_t base,
    const char *data,
    int off,
    int len);

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

/* returns a pointer to a table of instruction strings, unlike the
 * «static» insn and reg tables, this table can be invalidated after
 * each run of the disassembler.
 */
const char *bap_disasm_queue_asm_table(bap_disasm_type disasm);

/* returns a size of asm table  */
int bap_disasm_queue_asm_table_size(bap_disasm_type disasm);



/** Operations over a set of predicates  */

/* clears the set of predicates  */
void bap_disasm_predicates_clear(bap_disasm_type disasm, bap_disasm_insn_p_type p);



/* bap_disasm_predicates_push(disasm,p) adds predicate p to the set of
 * predicates */
void bap_disasm_predicates_push(bap_disasm_type disasm,
                        bap_disasm_insn_p_type p);

/* returns true, if the predicate is supported by the backend  */
int bap_disasm_predicate_is_supported(bap_disasm_type disasm,
                                       bap_disasm_insn_p_type p);

/* moves pc to the specfied address  */
void bap_disasm_set_offset(bap_disasm_type disasm, int offset);

/* get current pc  */
int bap_disasm_get_offset(bap_disasm_type disasm);

/* runs disassembler until one of the predicates evaluates to true or
 * until there is no more data. An empty set of predicates, always
 * evaluates to false.
 *
 * The condition check is performed _after_ each diassembly step. This
 * means, that if [is_true] is in a set of predicates, then each
 * invocation of the [bap_disasm_run] will disassemble exactly
 * one instruction (in assumption that there is enough space).
 *
 * In case of errors, an invalid instruction is pushed into queue,
 * and disassembly process is continued according to the current
 * set of predicates.
 */
void bap_disasm_run(bap_disasm_type disasm);


void bap_disasm_insns_clear(bap_disasm_type disasm);


/* returns the amount of instructions in the decompiler queue.  */
int bap_disasm_insns_size(bap_disasm_type disasm);

/** returns size in bytes of the insn @n  */
int bap_disasm_insn_size(bap_disasm_type disasm, int insn);


/** returns insn offset in an insn name table */
int bap_disasm_insn_name(bap_disasm_type disasm, int insn);

/** returns a unique code of the insn. The code identifies an
 * instruction in the set of instructions for the specified target.*/
int bap_disasm_insn_code(bap_disasm_type disasm, int insn);

/** returns an offset of the instruction in a memory region  */
int bap_disasm_insn_offset(bap_disasm_type disasm, int insn);


/* returns an offset of the insn disassembly string in the current asm
 * table */
int bap_disasm_insn_asm(bap_disasm_type disasm, int insn);

/* returns true if instruction satisfies predicate */
int bap_disasm_insn_satisfies(bap_disasm_type disasm,
                              int insn,
                              bap_disasm_insn_p_type p);

/* returns the amount of operands for a given instruction  */
int bap_disasm_insn_ops_size(bap_disasm_type disasm,
                             int insn);

/* returns the type of the operand  */
bap_disasm_op_type bap_disasm_insn_op_type(bap_disasm_type disasm,
                                           int insn,
                                           int op);

/* returns an offset in a registers name table */
int bap_disasm_insn_op_reg_name(bap_disasm_type disasm,
                                int insn,
                                int op);

/* returns a unique identifier of the register operand */
int bap_disasm_insn_op_reg_code(bap_disasm_type disasm,
                                int insn,
                                int op);

/* returns value of the integer immediate value */
int64_t bap_disasm_insn_op_imm_value(bap_disasm_type disasm,
                                     int insn,
                                     int op);

/* returns value of the floating point immediate value */
double bap_disasm_insn_op_fmm_value(bap_disasm_type disasm,
                                    int insn,
                                    int op);

/* returns a subinstruction of the instruction. It can be accessed
 * the same way as a "toplevel" instructions, and it is guaranteed,
 * that returned value is not in the set of valid instruction numbers for
 * the current queue */
int bap_disasm_insn_op_insn_value(bap_disasm_type disasm,
                                  int insn,
                                  int op);

#endif  /* BAP_DISASM_H */
