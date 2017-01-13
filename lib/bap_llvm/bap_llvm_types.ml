open Core_kernel.Std

module Section = struct
  type t = {
    name : string;
    addr: int64;
    size : int64;
  } [@@deriving fields]
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
  } [@@deriving fields]

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
  } [@@deriving fields]
end

type section = Section.t
type segment = Segment.t
type symbol = Symbol.t
