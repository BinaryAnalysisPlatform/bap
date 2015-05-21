open Core_kernel.Std
open Bap_types.Std

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
  val update : ('a,'b) cls -> 'a t -> 'b t -> 'a t
  val remove : ('a,_) cls -> 'a t -> tid -> 'a t
  val to_sequence : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t seq
  val map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t) -> 'a t
  val filter_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t option) -> 'a t
  val concat_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t list) -> 'a t
  val filter : ('a,'b) cls -> 'a t -> f:('b t -> bool) -> 'a t
  val next : ('a,'b) cls -> 'a t -> tid -> 'b t option
  val prev : ('a,'b) cls -> 'a t -> tid -> 'b t option
  val first : ('a,'b) cls -> 'a t -> 'b t option
  val last  : ('a,'b) cls -> 'a t -> 'b t option
  val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq
  val before : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq
  val append : ('a,'b) cls -> ?after:tid -> 'a t -> 'b t -> 'a t
  val prepend : ('a,'b) cls -> ?before:tid -> 'a t -> 'b t -> 'a t
  val length : ('a,'b) cls -> 'a t -> int
  val set_attr : 'a t -> 'b tag -> 'b -> 'a t
  val get_attr : 'a t -> 'b tag -> 'b option
  val del_attr : 'a t -> 'b tag -> 'a t
  val has_attr : 'a t -> 'b tag -> bool
end

module Ir_program : sig
  type t = program term
  val create : unit -> t
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
  val create : ?name:string -> unit -> t
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
  type elt =
    | Def of def term
    | Phi of phi term
    | Jmp of jmp term
  val create : unit -> t
  val split_while : t -> f:(def term -> bool) -> t * t
  val split_after : t -> def term -> t * t
  val split_before : t -> def term -> t * t
  val split_top : t -> t * t
  val split_bot : t -> t * t
  val elts : ?rev:bool -> t -> elt seq
  val dominated : t -> by:tid -> tid -> bool

  module Builder : sig
    type t
    val create : ?tid:tid -> ?phis:int -> ?defs:int -> ?jmps:int -> unit -> t
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
  val create : var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val with_lhs : t -> var -> t
  val with_rhs : t -> exp -> t

  include Regular with type t := t
end


module Ir_jmp : sig
  type t = jmp term
  val create : ?cond:exp -> jmp_kind -> t
  val create_call : ?cond:exp -> call -> t
  val create_goto : ?cond:exp -> label -> t
  val create_ret  : ?cond:exp -> label -> t
  val create_int  : ?cond:exp -> int -> tid -> t
  val kind : t -> jmp_kind
  val cond : t -> exp

  val with_cond : t -> exp -> t
  val with_kind : t -> jmp_kind -> t
  include Regular with type t := t
end

module Ir_phi : sig
  type t = phi term
  val create : var -> def term -> t
  val lhs : t -> var
  val defs : t -> def term seq
  val add_def : t -> def term -> t
  val remove_def : t -> tid -> t

  include Regular with type t := t
end

module Ir_arg : sig
  type t = arg term

  val create : ?intent:intent -> ?name:string -> typ -> t
  val var : t -> var
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
