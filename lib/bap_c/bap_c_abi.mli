open Bap.Std

(** [arg_intent t] infers argument intention based on its C type.  If
    an argument is passed by value, i.e., it is a c basic type, then
    it is an input argument. If an argument is a reference, but not a
    function, then it is input/output if any value, referenced by the
    argument is non-const. A reference to function always has the
    input intent. If an argyment is a structure or union, then it is
    input/output if any of its fields is input/output.
*)
val arg_intent : Bap_c_type.t -> intent

type param = Bap_c_data.t * exp

type proto = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
}

val create_proto :
  ?return:param ->
  ?hidden:(Bap_c_type.t * param) list -> param list -> proto

val create_api_processor : arch -> (Bap_c_type.proto -> proto) -> Bap_api.t

module Stack : sig
  (** [stack = create ?growsup arch] is a function that returns
      [n]'th stack slot *)
  val create : ?growsup:bool -> arch -> int -> exp
end
