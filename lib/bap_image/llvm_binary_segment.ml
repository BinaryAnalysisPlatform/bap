
type t = {
  name : string;
  offset : int64;
  addr: int64;
  size : int64;
  is_readable: bool;
  is_writable: bool;
  is_executable: bool;
} with fields
