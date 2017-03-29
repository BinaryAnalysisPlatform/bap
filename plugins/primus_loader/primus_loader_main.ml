open Core_kernel.Std
open Bap.Std
open Bap_primus.Std

module Param = struct
  let stack_size = 8 * 1024 * 1024
  let stack_base = Int64.(1L * 1024L * 1024L * 1024L)
end


let () = Primus.Machine.add_component (module Primus_loader_basic.Make(Param))
