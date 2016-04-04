open Bap.Std

(** registers x86 ABIs *)
val register : unit -> unit

(** register ABI resolver for the given arch. If default is not [Some abi],
    then this [abi] will be used if nothing else is prescribed by
    function attributes  *)
val register_resolver : ?default:string -> Arch.x86 -> unit
