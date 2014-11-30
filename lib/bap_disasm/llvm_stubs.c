#include <caml/mlvalues.h>
#include "llvm_disasm.h"


value bap_disasm_llvm_init_stub(value unit) {
    return Val_int(bap_disasm_llvm_init());
}
