open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_value

type 'a term with bin_io, compare, sexp
type program with bin_io, compare, sexp
type sub with bin_io, compare, sexp
type arg with bin_io, compare, sexp
type blk with bin_io, compare, sexp
type phi with bin_io, compare, sexp
type def with bin_io, compare, sexp
type jmp with bin_io, compare, sexp

type tid with bin_io, compare, sexp
type call with bin_io, compare, sexp

type label =
  | Direct of tid
  | Indirect of exp
with bin_io, compare, sexp

type jmp_kind =
  | Call of call
  | Goto of label
  | Ret  of label
  | Int  of int * tid
with bin_io, compare, sexp

type intent =
  | In
  | Out
  | Both
with bin_io, compare, sexp

type ('a,'b) cls

val sub_t : (program, sub) cls
val arg_t : (sub, arg) cls
val blk_t : (sub, blk) cls
val phi_t : (blk, phi) cls
val def_t : (blk, def) cls
val jmp_t : (blk, jmp) cls

module Tid : sig
  type t = tid
  val create : unit -> t
  include Regular with type t := t
end

module Term : sig
  type 'a t = 'a term
  val clone : 'a t -> 'a t
  val same : 'a t -> 'a t -> bool
  val name : 'a t -> string
  val tid : 'a t -> tid
  val find : ('a,'b) cls -> 'a t -> tid -> 'b t option
  val find_exn : ('a,'b) cls -> 'a t -> tid -> 'b t
  val update : ('a,'b) cls -> 'a t -> 'b t -> 'a t
  val remove : ('a,_) cls -> 'a t -> tid -> 'a t
  val change : ('a,'b) cls -> 'a t -> tid -> ('b t option -> 'b t option) -> 'a t
  val to_sequence : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t Sequence.t
  val enum : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t Sequence.t
  val map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t) -> 'a t
  val filter_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t option) -> 'a t
  val concat_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t list) -> 'a t
  val filter : ('a,'b) cls -> 'a t -> f:('b t -> bool) -> 'a t
  val next : ('a,'b) cls -> 'a t -> tid -> 'b t option
  val prev : ('a,'b) cls -> 'a t -> tid -> 'b t option
  val first : ('a,'b) cls -> 'a t -> 'b t option
  val last  : ('a,'b) cls -> 'a t -> 'b t option
  val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t Sequence.t
  val before : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t Sequence.t
  val append : ('a,'b) cls -> ?after:tid -> 'a t -> 'b t -> 'a t
  val prepend : ('a,'b) cls -> ?before:tid -> 'a t -> 'b t -> 'a t
  val length : ('a,'b) cls -> 'a t -> int
  val nth : ('a,'b) cls -> 'a t -> int -> 'b t option
  val nth_exn : ('a,'b) cls -> 'a t -> int -> 'b t
  val set_attr : 'a t -> 'b tag -> 'b -> 'a t
  val get_attr : 'a t -> 'b tag -> 'b option
  val del_attr : 'a t -> 'b tag -> 'a t
  val has_attr : 'a t -> 'b tag -> bool
end

module Ir_program : sig
  type t = program term
  val create : ?tid:tid -> unit -> t
  val lookup : (_,'b) cls -> t -> tid -> 'b term option
  val parent : ('a,'b) cls -> t -> tid -> 'a term option
  module Builder : sig
    type t
    val create : ?tid:tid  -> ?subs:int -> unit -> t
    val add_sub : t -> sub term -> unit
    val result : t -> program term
  end

  include Regular with type t := t
end

module Ir_sub : sig
  type t = sub term
  val create : ?tid:tid -> ?name:string -> unit -> t
  val name : t -> string
  val with_name : t -> string -> t
  module Builder : sig
    type t
    val create : ?tid:tid -> ?args:int -> ?blks:int -> ?name:string -> unit -> t
    val add_blk : t -> blk term -> unit
    val add_arg : t -> arg term -> unit
    val result : t -> sub term
  end
  include Regular with type t := t
end

module Ir_blk : sig
  type t = blk term
  type elt = [
    | `Def of def term
    | `Phi of phi term
    | `Jmp of jmp term
  ]
  val create : ?tid:tid -> unit -> t
  val split_while : t -> f:(def term -> bool) -> t * t
  val split_after : t -> def term -> t * t
  val split_before : t -> def term -> t * t
  val split_top : t -> t * t
  val split_bot : t -> t * t
  val elts : ?rev:bool -> t -> elt Sequence.t
  val map_exp :
    ?skip:[`phi | `def | `jmp] list ->
    t -> f:(exp -> exp) -> t
  val map_lhs :
    ?skip:[`phi | `def ] list ->
    t -> f:(var -> var) -> t
  val find_var : t -> var -> [
      | `Phi of phi term
      | `Def of def term
    ] option
  val defines_var : t -> var -> bool
  val uses_var : t -> var -> bool

  val dominated : t -> by:tid -> tid -> bool

  module Builder : sig
    type t
    val create : ?tid:tid -> ?phis:int -> ?defs:int -> ?jmps:int -> unit -> t
    val init :
      ?same_tid :bool ->
      ?copy_phis:bool ->
      ?copy_defs:bool ->
      ?copy_jmps:bool ->
      blk term -> t

    val add_def : t -> def term -> unit
    val add_jmp : t -> jmp term -> unit
    val add_phi : t -> phi term -> unit
    val add_elt : t -> elt -> unit
    val result  : t -> blk term
  end
  include Regular with type t := t
end

module Ir_def : sig
  type t = def term
  val create : ?tid:tid -> var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val with_lhs : t -> var -> t
  val with_rhs : t -> exp -> t
  val map_exp : t -> f:(exp -> exp) -> t
  include Regular with type t := t
end

module Ir_jmp : sig
  type t = jmp term
  val create      : ?tid:tid -> ?cond:exp -> jmp_kind -> t
  val create_call : ?tid:tid -> ?cond:exp -> call -> t
  val create_goto : ?tid:tid -> ?cond:exp -> label -> t
  val create_ret  : ?tid:tid -> ?cond:exp -> label -> t
  val create_int  : ?tid:tid -> ?cond:exp -> int -> tid -> t
  val kind : t -> jmp_kind
  val cond : t -> exp
  val with_cond : t -> exp -> t
  val with_kind : t -> jmp_kind -> t
  val map_exp : t -> f:(exp -> exp) -> t
  include Regular with type t := t
end

module Ir_phi : sig
  type t = phi term
  val create : ?tid:tid -> var -> tid -> exp -> t
  val of_list : ?tid:tid -> var -> (tid * exp) list -> t
  val lhs : t -> var
  val with_lhs : t -> var -> t
  val values : t -> (tid * exp) Sequence.t
  val update : t -> tid -> exp -> t
  val select : t -> tid -> exp option
  val remove : t -> tid -> t
  val select_or_unknown : t -> tid -> exp
  val map_exp : t -> f:(exp -> exp) -> t
  include Regular with type t := t
end

module Ir_arg : sig
  type t = arg term

  val create : ?tid:tid -> ?intent:intent -> var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val intent : t -> intent option
  val with_intent : t -> intent -> t
  val with_unknown_intent : t -> t
  include Regular with type t := t
end

module Call : sig
  type t = call
  val create : ?return:label -> target:label -> unit -> t
  val target : t -> label
  val return : t -> label option
  val with_target : t -> label -> t
  val with_return : t -> label -> t
  val with_noreturn : t -> t

  include Regular with type t := t
end

module Label : sig
  type t = label
  val create : unit -> t
  val direct : tid -> t
  val indirect : exp -> t
  val change : ?direct:(tid -> tid) -> ?indirect:(exp -> exp) -> t -> t
  include Regular with type t := t
end
