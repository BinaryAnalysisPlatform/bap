open Core_kernel.Std
open Bap.Std
(** reads symtab from a given file.
    File grammar:
    {[syms = sym, {sym};
      sym  = "(" name, addr, addr ")".]}

    Where the firs [addr] is the symbol start, end the
    last addr points to the next byte after the last.

    Example:
      (sub_AE8C 0xae8c 0xaecc)
      (sub_AED0 0xaed0 0xaf10)

    The file can be generated from IDA with the following script:

    Wait()
    fs = Functions(SegStart(ea), SegEnd(ea))
    for f in fs:
       print '(%s 0x%x 0x%x)' % (
         GetFunctionName(f),
         GetFunctionAttr(f, FUNCATTR_START),
         GetFunctionAttr(f, FUNCATTR_END))

    where [ea] is any address inside the section of interest.

*)


(** [read arch ic] reads (symbol, start, end) from input channel [ic] *)
val read : arch -> in_channel -> (string * addr * addr) list

(** [read_addrs ic] reads function start address list from input
    channel [ic] *)
val read_addrs : in_channel -> addr list

(** [write oc syms] writes the [syms] in format of (name, start, end) to
    output channel [oc] *)
val write : out_channel -> (string * addr * addr) list -> unit

(** [write oc addrs] writes function start addresses list [addrs] to
    output channel [oc] *)
val write_addrs : out_channel -> addr list -> unit
