open Bap.Std
open Core_kernel.Std

type truth_format = [
  | `unstripped_bin
  | `symbol_file ]


let format_of_filename f =
  if Filename.check_suffix f ".scm" then `symbol_file
  else `unstripped_bin

let of_truth truth ~testbin : addr seq Or_error.t =
  match format_of_filename truth with
  | `unstripped_bin -> Ground_truth.from_unstripped_bin truth
  | `symbol_file -> Ground_truth.from_symbol_file truth ~testbin

let of_tool tool ~testbin : addr seq Or_error.t =
  if tool = "bap-byteweight"
  then Find_starts.with_byteweight testbin
  else Find_starts.with_ida ~which_ida:tool testbin
