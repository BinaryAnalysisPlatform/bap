open Bap_knowledge

module KB = Knowledge

type cls
type +'a sort
type 'a t = (cls,'a sort) KB.cls KB.value

val cls : (cls,unit) Knowledge.cls

val empty : 'a sort -> 'a t
val sort : 'a t -> 'a sort


module Sort : sig
  type data = private Data
  type ctrl = private Ctrl
  type +'a t = 'a sort


  val data : string -> data t
  val ctrl : string -> ctrl t
  val top : unit t
  val bot : 'a t

  val both : 'a t -> 'a t -> 'a t
  val (&&) : 'a t -> 'a t -> 'a t
  val union : 'a t list -> 'a t
  val join : 'a t list -> 'b t list -> unit t

  val order : 'a t -> 'b t -> Knowledge.Order.partial


  val rreg : data t
  val wreg : data t
  val rmem : data t
  val wmem : data t
  val barr : data t


  val fall : ctrl t
  val jump : ctrl t
  val cjmp : ctrl t
end
