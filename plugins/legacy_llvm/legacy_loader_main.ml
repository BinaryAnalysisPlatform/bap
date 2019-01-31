open Core_kernel
open Bap.Std
include Self()

let () =
  match Bap_llvm_loader.init () with
  | Ok () -> ()
  | Error er -> eprintf "%s\n" (Error.to_string_hum er)
