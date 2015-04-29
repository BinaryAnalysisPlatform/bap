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

val read : ?demangle:Options.demangle -> filename:string -> arch -> mem -> string table

val read_addrset : string -> Addr.Set.t

val write_addrset : ?filename:string -> Addr.Set.t -> unit

val write : ?filename:string -> symbol table -> unit
