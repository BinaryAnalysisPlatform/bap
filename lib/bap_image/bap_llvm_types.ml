open Core_kernel.Std

(** LLVM backend uses natural names for sections and segments, as per
    ELF notation. They will be mapped to BAP notation later.
*)


module Section = struct
  type t = {
    name : string;
    addr: int64;
    size : int64;
  } with fields
end


module Segment = struct
  type t = {
    name : string;
    offset : int64;
    addr: int64;
    size : int64;
    is_readable: bool;
    is_writable: bool;
    is_executable: bool;
  } with fields

end

module Symbol = struct
  type kind =
    | Unknown
    | Data
    | Debug
    | File
    | Function
    | Other

  type t = {
    name : string;
    kind : kind;
    addr : int64;
    size : int64;
  } with fields
end
