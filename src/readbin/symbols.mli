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

(** [read ?demangle ic arch mem] reads symbol table from input channel
    [ic] *)
val read : ?demangle:Options.demangle -> in_channel -> arch -> mem -> string table

(** [read_addrset ic] reads function start address set from input
    channel [ic] *)
val read_addrset : in_channel -> Addr.Set.t

(** [write oc sym] writes function start address set from symbol table
    [sym] to output channel [oc] *)
val write : out_channel -> Image.sym table -> unit

(** [write oc addrset] writes function start addresses [addrset] to
    output channel [oc] *)
val write_addrset : out_channel -> Addr.Set.t -> unit
