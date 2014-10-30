open Core_kernel.Std
open Bap_types.Std

type perm = { r : bool; w : bool; x : bool}

module Section = struct
  type t = {
    name: string;
    addr: addr;         (** virtual address of the first byte *)
    perm: perm;         (** section's permissions  *)
    off:  int;          (** offset in a data  *)
    size: int;          (** size of section in a data  *)
    data: Bigstring.t;  (** section data  *)
  } with fields
end

module Sym = struct
  type t = {
    name : string option;
    kind: [`undef | `func];
    addr: addr;
    size: int option;
  } with fields
end

module Img = struct
  type t = {
    arch : arch;
    addr_size: Word_size.t;
    endian   : endian;
    entry    : addr;
    sections : Section.t array;
    symbols  : Sym.t array;
  } with fields
end

type t = {
  (** [read data imag]  *)
  of_data: Bigstring.t -> Img.t option;
  to_data: (Img.t -> Bigstring.t) option;
}
