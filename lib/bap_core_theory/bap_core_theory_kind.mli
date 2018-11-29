open Caml.Format


type data = private Data_Effect
type ctrl = private Ctrl_Effect

type 'a t

val data : data t
val ctrl : ctrl t
val unit : unit t

val pp : formatter -> 'a t -> unit
val name : 'a t -> string
