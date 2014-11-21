#include "disasm.hpp"
#include "llvm_disasm.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main() {
    const char *bits = "\x53\xe8\xfa\x03\x00\x00\x00";
    char buffer[0x100];
    int r = bap_disasm_llvm_init();
    assert(r == 0);
    bap_disasm_type d = bap_disasm_create("llvm", "x86_64", "", 0);
    assert (d >= 0);
    printf("bits (%p) '%s'\n", bits, bits);

    const char *names = bap_disasm_insn_table_ptr(d);
    const char *regs = bap_disasm_reg_table_ptr(d);

    // bap_disasm_set_memory(d, 0, bits, 0, 7);
    bap_disasm_set_memory(d, 0x415ad1, (char *)0x400000, 0x15ad1, 0x1000);
        bap_disasm_predicates_push(d, is_true);
    for (int i = 0; i < 1000; i++) {
        bap_disasm_run(d);
        if (!bap_disasm_insn_satisfies(d,i, is_invalid)) {
            bap_disasm_insn_asm_copy(d, i, buffer);
            buffer[bap_disasm_insn_asm_size(d, i)] = '\x00';
            printf("%-32s\t# %s ", buffer, &names[bap_disasm_insn_name(d,i)]);
            for (int j = 0; j < bap_disasm_insn_ops_size(d,i); ++j) {
                switch (bap_disasm_insn_op_type(d,i,j)) {
                case bap_disasm_op_reg:
                    printf("%s ", regs + bap_disasm_insn_op_reg_name(d,i,j));
                    break;
                case bap_disasm_op_imm:
                    printf("%ld ", bap_disasm_insn_op_imm_value(d,i,j));
                    break;
                case bap_disasm_op_fmm:
                    printf("%g ", bap_disasm_insn_op_fmm_value(d,i,j));
                    break;
                case bap_disasm_op_insn: {
                    int sub = bap_disasm_insn_op_insn_value(d,i,j);
                    printf("%s ", names + bap_disasm_insn_name(d,sub));
                    break;
                }
                default:
                    assert(false);
                }
            }
            printf("\n");
        } else {
            int n = bap_disasm_insn_size(d,i);
            printf("... skipping %d byte%s\n", n, (n>1) ? "s" : "");
        }
    }

}
