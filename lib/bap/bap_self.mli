open Format
open Core_kernel.Std

module Create() : sig
  val name : string
  val version : string
  val doc : string
  val argv : string array

  val debug   : ('a,formatter,unit) format -> 'a
  val info    : ('a,formatter,unit) format -> 'a
  val warning : ('a,formatter,unit) format -> 'a
  val error   : ('a,formatter,unit) format -> 'a

  module Param : sig
    type 'a t

    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type 'a converter = 'a parser * 'a printer

    val int : int converter
    val bool : bool converter
    val string : string converter

    val create :
      'a converter -> default:'a ->
      ?docv:string -> doc:string -> name:string -> 'a t

    val flag :
      ?docv:string -> doc:string -> name:string -> bool t

    val extract : unit -> 'a t -> 'a

    type manpage_block = [
        `I of string * string |
        `Noblank |
        `P of string |
        `Pre of string |
        `S of string
    ] list

    val manpage : manpage_block -> unit

  end

end
