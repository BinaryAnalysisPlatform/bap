external ghidra_init : string -> bool -> int = "disasm_ghidra_init_stub"

let init ?(paths=["/usr/share/ghidra"]) ?(print_targets=false) () =
  let paths = String.concat ":" paths in
  if ghidra_init paths print_targets < 0 then
    failwith "failed to initialize ghidra backend. See stderr for information"
