open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

type insn = Bap_disasm_insn.t

type t = (mem * insn option) list

val sweep : ?backend:string -> arch -> mem -> t Or_error.t

module With_exn : sig
  val sweep : ?backend:string -> arch -> mem -> t
end
