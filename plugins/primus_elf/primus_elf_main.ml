open Bap.Std
open Primus.Std

module Param = struct
  let stack_size = 8 * 1024 * 1024
  let stack_base = 0x8000_000L
end


let () = Machine.add_component (module Primus_elf_loader.Make(Param))
