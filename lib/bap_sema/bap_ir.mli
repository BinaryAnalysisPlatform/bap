open Core_kernel.Std
open Bap_types.Std

type 'a term

type program
type sub
type arg
type blk
type phi
type def
type jmp

type tid
type call

type label =
  | Direct of tid
  | Indirect of exp

type jmp_kind =
  | Call of call
  | Goto of label

type ('a,'b) cls

val sub_t : (program, sub) cls
val arg_t : (sub, arg) cls
val blk_t : (sub, blk) cls
val phi_t : (blk, phi) cls
val def_t : (blk, def) cls
val jmp_t : (blk, jmp) cls

module Tid : Regular with type t = tid

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
  val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq
  val before : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq
  val append : ('a,'b) cls -> ?after:tid -> 'a t -> 'b t -> 'a t
  val prepend : ('a,'b) cls -> ?before:tid -> 'a t -> 'b t -> 'a t
end

module Program : sig
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

module Sub : sig
  type t = sub term
  val create : ?name:string -> unit -> t
  val name : t -> string
  module Builder : sig
    type t
    val create : ?tid:tid -> ?args:int -> ?blks:int -> ?name:string -> unit -> t
    val add_blk : t -> blk term -> unit
    val add_arg : t -> arg term -> unit
    val result : t -> sub term
  end
  include Regular with type t := t
end

module Blk : sig
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

module Def : sig
  type t = def term
  val create : var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val with_lhs : t -> var -> t
  val with_rhs : t -> exp -> t

  include Regular with type t := t
end


module Jmp : sig
  type t = jmp term
  val create_call : ?cond:exp -> call -> t
  val create_goto : ?cond:exp -> label -> t
  val kind : t -> jmp_kind
  val cond : t -> exp

  val with_cond : t -> exp -> t
  val with_kind : t -> jmp_kind -> t
  include Regular with type t := t
end

module Phi : sig
  type t = phi term
  val create : var -> def term -> t
  val lhs : t -> var
  val defs : t -> def term seq
  val add_def : t -> def term -> t
  val remove_def : t -> tid -> t

  include Regular with type t := t
end

module Arg : sig
  type t = arg term

  val create : ?name:string -> typ -> t
  val of_var : var -> t
  val to_var : t -> var
  val name : t -> string
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
  val direct : tid -> t
  val indirect : exp -> t
  val change : ?direct:(tid -> tid) -> ?indirect:(exp -> exp) -> t -> t
  include Regular with type t := t
end
