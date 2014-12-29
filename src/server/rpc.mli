open Core_kernel.Std
open Bap.Std

type request
type response
type resource = string
type target
type id


module Id : Identifiable with type t := id


module Target : sig
  type t = target

  val arm :  Disasm.Arm.Insn.t -> Disasm.Arm.Op.t list -> t

end

(* module Request : sig *)
(*   type t = request *)
(*   val id : t -> id *)

(*   val accept : t -> *)
(*     init:(string -> 'a) -> *)
(*     load_file:(?loader:string -> resource -> 'a) -> *)
(*     load_memory_chunk:(?addr:addr -> arch -> resource -> 'a) -> *)
(*     get_insns:(?stop_conditions:(Disasm.kind list) -> id -> 'a) -> *)
(*     get_resource:(id -> 'a) -> 'a *)
(* end *)


module Response : sig
  type t = response
  type msg
  type insn

  (** creates a response to the request with the [id]  *)
  val create : id -> msg -> t

  val error : id -> [`Critical | `Error | `Warning] -> string -> msg

  val capabilities : (* unimplemented *) msg



  val image :
    img:resource ->
    secs:resource list ->
    syms:resource list ->
    Uri.t List1.t ->
    Image.t -> msg


  val section :
    img:resource ->
    sec:resource ->
    mem:resource ->
    Uri.t List1.t  ->
    Section.t ->
    msg

  val symbol :
    sec:resource ->
    sym:resource ->
    mem:resource list ->
    Uri.t List1.t ->
    Symbol.t -> msg

  val memory :
    ?sec:resource ->
    ?sym:resource ->
    mem:resource ->
    Uri.t List1.t ->
    Memory.t -> msg

  val insn :
    ?target:target -> ?bil:stmt list ->
    mem_id:resource -> Disasm.Basic.full_insn -> insn

  val insns : insn list -> msg

  val images : resource list -> msg
  val sections : resource list -> msg
  val symbols  : resource list -> msg
  val chunks : resource list -> msg
end
