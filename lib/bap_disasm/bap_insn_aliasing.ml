open Core_kernel
open Bap_types.Std

module Dis = Bap_disasm_basic
module Memory = Bap_memory

let is_exec_ok gmem_min gmem_max lmem  =
  Addr.((Memory.min_addr lmem) >= gmem_min) &&
  Addr.((Memory.max_addr lmem) <= gmem_max)

let targ_in_mem gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr < gmem_max)
