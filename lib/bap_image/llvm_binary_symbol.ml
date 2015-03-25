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
