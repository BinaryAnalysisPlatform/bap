open Format

module Create() : sig
  val name : string
  val version : string
  val doc : string
  val argv : string array

  val debug   : ('a,formatter,unit) format -> 'a
  val info    : ('a,formatter,unit) format -> 'a
  val warning : ('a,formatter,unit) format -> 'a
  val error   : ('a,formatter,unit) format -> 'a

  module Config : sig
    val get : string -> string option
    val set : name:string -> data:string -> unit
    val options : unit -> (string * string) list
  end
end
