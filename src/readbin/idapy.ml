open Core_kernel.Std
open Bap.Std
open Project

let extract_symbols output = sprintf "
from idautils import *
with open('%s', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%%s 0x%%x 0x%%x)\\n' %% (
                GetFunctionName(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))

idc.Exit(0)" output

let addr take mem =
  sprintf "0x%s" @@ Addr.string_of_value (take mem)

let extract_script map =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "from idautils import *\n";
  Memmap.iter map ~f:(fun tag ->
      match Tag.value python tag with
      | Some line -> Buffer.add_string buf line
      | None -> ());
  Buffer.contents buf
