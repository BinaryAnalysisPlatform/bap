open Core_kernel.Std
open Bap.Std
open Program_visitor

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

let annotate_ida p =
  let buf = Buffer.create 64 in
  Buffer.add_string buf "from idautils import *\n";
  Memmap.to_sequence p.annots |> Seq.iter ~f:(fun (mem,(tag,script)) ->
      if tag = "idapy" then
        Buffer.add_substitute buf (function
            | "min_addr" -> addr Memory.min_addr mem
            | "max_addr" -> addr Memory.max_addr mem
            | "mem_size" -> Int.to_string (Memory.length mem)
            | s -> sprintf
                     "raise RuntimeError('bad substitution: %s')" s)
          script);
  Buffer.contents buf
