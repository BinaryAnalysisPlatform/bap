open Core_kernel.Std

external init : unit -> int = "bap_disasm_llvm_init_stub" "noalloc"


let () =
  let r = init () in
  if r < 0 then
    eprintf "LLVM initialization failed with error %d" r
