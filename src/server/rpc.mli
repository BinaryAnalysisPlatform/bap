(* open Core_kernel.Std *)
(* open Bap.Std *)

(* type request *)
(* type response *)
(* type resource *)
(* type target *)
(* type id *)

(* module Id : Identifiable with type t := id *)


(* module Target : sig *)
(*   type t = target *)

(*   val arm :  Disasm.Arm.Insn.t -> Disasm.Arm.Op.t list -> t *)

(* end *)

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

(* module Resource : sig *)
(*   type t = resource *)

(* end *)

(* module Response : sig *)
(*   type t = response *)

(*   val insn : *)
(*     ?target:target -> ?bil:stmt list -> *)
(*     mem_id:id -> Disasm.Basic.full_insn -> t *)

(*   val section : Section.t -> t *)

(* end *)
