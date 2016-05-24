open Core_kernel.Std
open Bap.Std
open Project

let extract_symbols = "
from idautils import *
from idaapi import *

def func_name_propagate_thunk(ea):
    func = get_func(ea)
    temp_ptr = ea_pointer()
    ea_new = BADADDR
    if func.flags & FUNC_THUNK == FUNC_THUNK:
        ea_new = calc_thunk_func_target(func, temp_ptr.cast())
    if ea_new != BADADDR:
        ea = ea_new
    return get_func_name2(ea)

Wait()
with open('$output', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%%s 0x%%x 0x%%x)\\n' %% (
                func_name_propagate_thunk(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))

idc.Exit(0)"

let addr take mem =
  sprintf "0x%s" @@ Addr.string_of_value (take mem)

