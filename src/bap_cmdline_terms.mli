open Cmdliner
open Bap_options

val filename : string Term.t
val loader : source -> string Term.t
val disassembler : unit -> string Term.t
val list_formats : bool Term.t
val list_formats_doc : string
val dump_formats : unit -> fmt_spec list Term.t
val source_type : source Term.t
val verbose : bool Term.t
val brancher : source -> string option Term.t
val symbolizers : source -> string list Term.t
val rooters : source -> string list Term.t
val symbols : source -> string list Term.t
val reconstructor : source -> string option Term.t

val load : string list Term.t
val load_path : string list Term.t
val list_plugins : bool Term.t
val disable_plugin : string list Term.t
val no_auto_load : bool Term.t

val loader_options : string list
val common_loader_options : Manpage.block list
val options_for_passes    : Manpage.block list
