#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <string.h>

#include "ghidra_disasm.h"

value disasm_ghidra_init_stub(value paths, value print_targets) {
    CAMLparam2(paths, print_targets);
    char *s = strdup(String_val(paths));
    int r = Val_int(disasm_ghidra_init(s, Bool_val(print_targets)));
    free(s);
    CAMLreturn(r);
}
