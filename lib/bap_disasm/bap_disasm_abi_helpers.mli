(** A set of helper functions and classes  *)
open Bap_types.Std
open Bap_image_std
open Bap_disasm_abi


(** [merge abis] create an abi that tries to take best from all
    provided abi. If the input list is empty, then the stub abi
    will be returned. *)
val merge : abi list -> abi

val merge_id : string list -> string list -> string list

(** ABI that understands nothing. All methods are dump stubs. *)
class stub : abi

val to_string : arch -> string list -> string

(**/**)
val create_abi_getter :
  abi_constructor list ref ->
  ?merge:(abi list -> abi) -> (** defaults to [merge]  *)
  abi_constructor
