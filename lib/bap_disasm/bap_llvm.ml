external init_ : unit -> int = "bap_disasm_llvm_init_stub" "noalloc"


let init () =
  let r = init_ () in
  if r < 0 then (failwith "llvm backend failed to initialize");
