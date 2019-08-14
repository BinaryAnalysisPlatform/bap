open Bap.Std
open Bap_core_theory

type context
val context : context KB.obj KB.t
val arch : (context, arch option) KB.slot


module Core : Theory.Core
module Core_with_fp_emulation  : Theory.Core
