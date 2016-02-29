open Core_kernel.Std
open Bap.Std
open Project

let extract_symbols = "
from idautils import *
Wait()
with open('$output', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%%s 0x%%x 0x%%x)\\n' %% (
                GetFunctionName(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))

idc.Exit(0)"

let addr take mem =
  sprintf "0x%s" @@ Addr.string_of_value (take mem)

