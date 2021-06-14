#include <caml/mlvalues.h>

#include "ghidra_disasm.h"

value disasm_ghidra_init_stub(value unit) {
    return Val_int(disasm_ghidra_init());
}
