open Core_kernel
open Bap.Std
open Bap_primus_types

val pc_change : addr observation
val halting : unit observation
val interrupt : int observation
val division_by_zero : unit observation
val segfault : addr observation
val pagefault : addr observation
val cfi_violation : addr observation

val loading : value observation
val loaded : (value * value) observation
val storing : value observation
val stored : (value * value) observation
val filling : (value * int * int) observation
val filled : (value * int * int) observation

val reading : var observation
val read : (var * value) observation
val writing : var observation
val written : (var * value) observation
val jumping : (value * value) observation
val eval_cond : value observation
val undefined : value observation
val const : value observation

val binop : ((binop * value * value) * value) observation
val unop : ((unop * value) * value) observation
val cast : ((cast * int * value) * value) observation
val extract : ((int * int * value) * value) observation
val concat : ((value * value) * value) observation
val ite : ((value * value * value) * value) observation


val enter_exp : exp observation
val leave_exp : exp observation


val enter_term : tid observation
val leave_term : tid observation

val enter_pos : pos observation
val leave_pos : pos observation

val enter_top : program term observation
val enter_sub : sub term observation
val enter_arg : arg term observation
val enter_blk : blk term observation
val enter_phi : phi term observation
val enter_def : def term observation
val enter_jmp : jmp term observation

val leave_top : program term observation
val leave_sub : sub term observation
val leave_arg : arg term observation
val leave_blk : blk term observation
val leave_phi : phi term observation
val leave_def : def term observation
val leave_jmp : jmp term observation


type exn += Halt
type exn += Division_by_zero
type exn += Segmentation_fault of addr
type exn += Cfi_violation of addr

val division_by_zero_handler : string
val pagefault_handler : string
val cfi_violation_handler : string


module Make (Machine : Machine) : sig
  type 'a m = 'a Machine.t
  val halt : never_returns m
  val interrupt : int -> unit m
  val pc : addr m
  val pos : pos m
  val sub : sub term -> unit m
  val blk : blk term -> unit m
  val exp : exp -> value m
  val get : var -> value m
  val set : var -> value -> unit m
  val binop : binop -> value -> value -> value m
  val unop : unop -> value -> value m
  val cast : cast -> int -> value -> value m
  val concat : value -> value -> value m
  val extract : hi:int -> lo:int -> value -> value m
  val const : word -> value m
  val load : value -> endian -> size -> value m
  val store : value -> value -> endian -> size -> unit m
  val fill : value -> len:int -> int -> unit m
end


module Init (Machine : Machine) : sig
  val run : unit -> unit Machine.t
end
