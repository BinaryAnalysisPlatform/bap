open Core_kernel
open Regular.Std
open Bap_core_theory
open Bap_common
open Bap_bil
open Bap_value
open Bap_visitor
open Bap_core_theory

type tid = Theory.Label.t
[@@deriving bin_io, compare, sexp]


type 'a term [@@deriving bin_io, compare, sexp]
type program [@@deriving bin_io, compare, sexp]
type nil [@@deriving bin_io, compare, sexp]
type sub [@@deriving bin_io, compare, sexp]
type arg [@@deriving bin_io, compare, sexp]
type blk [@@deriving bin_io, compare, sexp]
type phi [@@deriving bin_io, compare, sexp]
type def [@@deriving bin_io, compare, sexp]
type jmp [@@deriving bin_io, compare, sexp]
type call [@@deriving bin_io, compare, sexp]

type label =
  | Direct of tid
  | Indirect of exp
[@@deriving bin_io, compare, sexp]

type jmp_kind =
  | Call of call
  | Goto of label
  | Ret  of label
  | Int  of int * tid
[@@deriving bin_io, compare, sexp]

type intent =
  | In
  | Out
  | Both
[@@deriving bin_io, compare, sexp]

type ('a,'b) cls

val program_t : (nil,program) cls
val sub_t : (program, sub) cls
val arg_t : (sub, arg) cls
val blk_t : (sub, blk) cls
val phi_t : (blk, phi) cls
val def_t : (blk, def) cls
val jmp_t : (blk, jmp) cls

module Tid : sig
  type t = tid
  val create : unit -> t
  val set_name : t -> string -> unit
  val name : t -> string
  val from_string : string -> tid Or_error.t
  val from_string_exn : string -> tid
  val (!!) : string -> tid
  include Regular.S with type t := t
end

module Term : sig
  type 'a t = 'a term

  val slot : (_ Theory.Program.Semantics.cls, blk term list) KB.slot

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
  val attrs : 'a t -> Dict.t
  val get_attr : 'a t -> 'b tag -> 'b option
  val del_attr : 'a t -> 'b tag -> 'a t
  val has_attr : 'a t -> 'b tag -> bool
  val with_attrs : 'a t -> Dict.t -> 'a t

  val origin : tid tag
  val synthetic : unit tag
  val live : unit tag
  val dead : unit tag
  val visited : unit tag
  val precondition : exp tag
  val invariant : exp tag
  val postcondition : exp tag

  class mapper : object
    inherit exp_mapper
    method run : program term -> program term
    method map_term : 't 'p. ('p,'t) cls -> 't term -> 't term
    method map_sub : sub term -> sub term
    method map_arg : arg term -> arg term
    method map_blk : blk term -> blk term
    method map_phi : phi term -> phi term
    method map_def : def term -> def term
    method map_jmp : jmp term -> jmp term
  end

  class ['a] visitor : object
    inherit ['a] exp_visitor

    method enter_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a
    method visit_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a
    method leave_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a

    method enter_program : program term -> 'a -> 'a
    method run           : program term -> 'a -> 'a
    method leave_program : program term -> 'a -> 'a

    method enter_sub : sub term -> 'a -> 'a
    method visit_sub : sub term -> 'a -> 'a
    method leave_sub : sub term -> 'a -> 'a

    method enter_blk : blk term -> 'a -> 'a
    method visit_blk : blk term -> 'a -> 'a
    method leave_blk : blk term -> 'a -> 'a

    method enter_arg : arg term -> 'a -> 'a
    method visit_arg : arg term -> 'a -> 'a
    method leave_arg : arg term -> 'a -> 'a

    method enter_phi : phi term -> 'a -> 'a
    method visit_phi : phi term -> 'a -> 'a
    method leave_phi : phi term -> 'a -> 'a

    method enter_def : def term -> 'a -> 'a
    method visit_def : def term -> 'a -> 'a
    method leave_def : def term -> 'a -> 'a

    method enter_jmp : jmp term -> 'a -> 'a
    method visit_jmp : jmp term -> 'a -> 'a
    method leave_jmp : jmp term -> 'a -> 'a
  end

  val switch : ('p,'t) cls ->
    program:(program term -> 'a) ->
    sub:(sub term -> 'a) ->
    arg:(arg term -> 'a) ->
    blk:(blk term -> 'a) ->
    phi:(phi term -> 'a) ->
    def:(def term -> 'a) ->
    jmp:(jmp term -> 'a) -> 't term -> 'a

  val proj : ('p,'t) cls ->
    ?program:(program term -> 'a option) ->
    ?sub:(sub term -> 'a option) ->
    ?arg:(arg term -> 'a option) ->
    ?blk:(blk term -> 'a option) ->
    ?phi:(phi term -> 'a option) ->
    ?def:(def term -> 'a option) ->
    ?jmp:(jmp term -> 'a option) ->
    't term -> 'a option

  val cata : ('p,'t) cls -> init:'a ->
    ?program:(program term -> 'a) ->
    ?sub:(sub term -> 'a) ->
    ?arg:(arg term -> 'a) ->
    ?blk:(blk term -> 'a) ->
    ?phi:(phi term -> 'a) ->
    ?def:(def term -> 'a) ->
    ?jmp:(jmp term -> 'a) ->
    't term -> 'a
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
  val pp_slots : string list -> Format.formatter -> t -> unit
  include Regular.S with type t := t
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

  val aliases : string list tag
  val const : unit tag
  val pure : unit tag
  val stub : unit tag
  val extern : unit tag
  val leaf : unit tag
  val malloc : unit tag
  val noreturn : unit tag
  val returns_twice : unit tag
  val nothrow : unit tag
  val entry_point : unit tag
  val pp_slots : string list -> Format.formatter -> t -> unit
  include Regular.S with type t := t
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
  val map_elts :
    ?phi:(phi term -> phi term) ->
    ?def:(def term -> def term) ->
    ?jmp:(jmp term -> jmp term) -> blk term -> blk term
  val substitute :
    ?skip:[`phi | `def | `jmp] list ->
    t -> exp -> exp -> t
  val map_lhs :
    ?skip:[`phi | `def ] list ->
    t -> f:(var -> var) -> t
  val find_var : t -> var -> [
      | `Phi of phi term
      | `Def of def term
    ] option
  val defines_var : t -> var -> bool
  val uses_var : t -> var -> bool
  val free_vars : t -> Bap_var.Set.t
  val occurs : t -> after:tid -> tid -> bool

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
  val pp_slots : string list -> Format.formatter -> t -> unit
  include Regular.S with type t := t
end

module Ir_def : sig
  type t = def term

  val reify : ?tid:tid ->
    'a Theory.var ->
    'a Theory.Sort.exp KB.value -> t

  val var : t -> unit Theory.var
  val value : t -> unit Theory.Sort.exp KB.value

  val create : ?tid:tid -> var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val with_lhs : t -> var -> t
  val with_rhs : t -> exp -> t
  val map_exp : t -> f:(exp -> exp) -> t
  val substitute : t -> exp -> exp -> t
  val free_vars : t -> Bap_var.Set.t
  val pp_slots : string list -> Format.formatter -> t -> unit

  include Regular.S with type t := t
end

module Ir_jmp : sig
  type t = jmp term
  type role


  val resolved : ?tid:tid ->
    ?cnd:Theory.Bool.t Theory.Sort.exp KB.value ->
    Theory.label -> role -> t

  val indirect : ?tid:tid ->
    ?cnd:Theory.Bool.t Theory.Sort.exp KB.value ->
    'a Theory.Bitv.t Theory.Sort.exp KB.value ->
    t

  val guard : t -> Theory.Bool.t Theory.Sort.exp KB.value option

  val value : t -> unit Theory.Sort.exp KB.Class.abstract KB.value

  val links : t -> (Theory.label * role) seq

  val link : t -> Theory.label -> role -> t

  val with_guard : t -> Theory.Bool.t Theory.Sort.exp KB.value option -> t

  module Role : sig
    type t = role

    val named : string -> role
    val name : role -> string

    val unknown : role
    val local : role
    val call : role
    val return : role
    val shortcut : role
  end

  val create      : ?tid:tid -> ?cond:exp -> jmp_kind -> t
  val create_call : ?tid:tid -> ?cond:exp -> call -> t
  val create_goto : ?tid:tid -> ?cond:exp -> label -> t
  val create_ret  : ?tid:tid -> ?cond:exp -> label -> t
  val create_int  : ?tid:tid -> ?cond:exp -> int -> tid -> t
  val kind : t -> jmp_kind
  val cond : t -> exp
  val with_cond : t -> exp -> t
  val with_kind : t -> jmp_kind -> t
  val exps : t -> exp Sequence.t
  val map_exp : t -> f:(exp -> exp) -> t
  val substitute : t -> exp -> exp -> t
  val free_vars : t -> Bap_var.Set.t
  val pp_slots : string list -> Format.formatter -> t -> unit

  include Regular.S with type t := t
end

module Ir_phi : sig
  type t = phi term

  val reify : ?tid:tid ->
    'a Theory.var ->
    (tid * 'a Theory.Sort.exp KB.value) list ->
    t

  val var : t -> unit Theory.var
  val options : t -> (tid * unit Theory.Sort.exp KB.value) seq

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
  val substitute : t -> exp -> exp -> t
  val free_vars : t -> Bap_var.Set.t
  val pp_slots : string list -> Format.formatter -> t -> unit
  include Regular.S with type t := t
end

module Ir_arg : sig
  type t = arg term

  val reify : ?tid:tid -> ?intent:intent ->
    'a Theory.var ->
    'a Theory.Sort.exp KB.value -> t

  val var : t -> unit Theory.var
  val value : t -> unit Theory.Sort.exp KB.value

  val create : ?tid:tid -> ?intent:intent -> var -> exp -> t
  val lhs : t -> var
  val rhs : t -> exp
  val intent : t -> intent option
  val with_intent : t -> intent -> t
  val with_unknown_intent : t -> t

  val alloc_size : unit tag
  val format : string tag
  val warn_unused : unit tag
  val restricted : unit tag
  val nonnull : unit tag
  val pp_slots : string list -> Format.formatter -> t -> unit

  module Intent : sig
    val slot : ('a Theory.Sort.exp KB.Class.abstract, intent option) KB.slot
  end

  include Regular.S with type t := t
end

module Call : sig
  type t = call
  val create : ?return:label -> target:label -> unit -> t
  val target : t -> label
  val return : t -> label option
  val with_target : t -> label -> t
  val with_return : t -> label -> t
  val with_noreturn : t -> t

  include Regular.S with type t := t
end

module Label : sig
  type t = label
  val create : unit -> t
  val direct : tid -> t
  val indirect : exp -> t
  val change : ?direct:(tid -> tid) -> ?indirect:(exp -> exp) -> t -> t
  include Regular.S with type t := t
end
