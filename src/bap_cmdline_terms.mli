open Cmdliner

val filename : string Term.t
val symsfile : string option Term.t
val loader : string Term.t
val cfg_format : [> `with_asm | `with_bil | `with_name ] list Term.t
val output_phoenix : string option Term.t
val output_dump : [> `with_asm | `with_bil | `with_bir ] list Term.t
val dump_symbols : string option option Term.t
val demangle : [ `internal | `program of string ] option Term.t
val no_resolve : bool Term.t
val keep_alive : bool Term.t
val no_inline : bool Term.t
val keep_consts : bool Term.t
val no_optimizations : bool Term.t
val binaryarch : string option Term.t
val verbose : bool Term.t
val bw_disable : bool Term.t
val bw_length : int Term.t
val bw_threshold : float Term.t
val print_symbols : [> `with_addr | `with_name | `with_size ] list Term.t
val use_ida : string option option Term.t
val sigsfile : string option Term.t
val emit_ida_script : string option Term.t
val load : string list Term.t
val load_path : string list Term.t
val emit_attr : string list Term.t
