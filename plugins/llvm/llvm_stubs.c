#include <caml/mlvalues.h>
#include "llvm_disasm.h"


value disasm_llvm_init_stub(value unit) {
    return Val_int(disasm_llvm_init());
}
