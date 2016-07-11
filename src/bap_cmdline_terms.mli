open Bap.Std
open Frontend
open Bap_options

val filename : string Config.param
val loader : string Config.param
val disassembler : string Config.param
val list_formats : bool Config.param
val list_formats_doc : string
val dump_formats : fmt_spec list Config.param
val source_type : source Config.param
val verbose : bool Config.param
val brancher : string option Config.param
val symbolizers : string list Config.param
val rooters : string list Config.param
val symbols : string list Config.param
val reconstructor : string option Config.param

val options_for_passes    : Config.manpage_block list
