open Format
open Core_kernel.Std
open Bap_future.Std

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
    val version : string
    val datadir : string
    val libdir : string
    val confdir : string

    type 'a param

    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type 'a converter

    val converter : 'a parser -> 'a printer -> 'a -> 'a converter

    val param :
      'a converter -> ?default:'a ->
      ?docv:string -> ?doc:string -> string -> 'a param

    val param_all :
      'a converter -> ?default:'a list ->
      ?docv:string -> ?doc:string -> string -> 'a list param

    val flag :
      ?docv:string -> ?doc:string -> string -> bool param

    val determined : 'a param -> 'a future

    type reader = {get : 'a. 'a param -> 'a}
    val when_ready : (reader -> unit) -> unit

    type manpage_block = [
      | `I of string * string
      | `Noblank
      | `P of string
      | `Pre of string
      | `S of string
    ]

    val manpage : manpage_block list -> unit

    val bool : bool converter
    val char : char converter
    val int : int converter
    val nativeint : nativeint converter
    val int32 : int32 converter
    val int64 : int64 converter
    val float : float converter
    val string : string converter
    val enum : (string * 'a) list -> 'a converter
    val file : string converter
    val dir : string converter
    val non_dir_file : string converter
    val list : ?sep:char -> 'a converter -> 'a list converter
    val array : ?sep:char -> 'a converter -> 'a array converter
    val pair : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter
    val t2 : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter
    val t3 : ?sep:char -> 'a converter -> 'b converter -> 'c converter ->
      ('a * 'b * 'c) converter
    val t4 : ?sep:char -> 'a converter -> 'b converter -> 'c converter ->
      'd converter -> ('a * 'b * 'c * 'd) converter
    val some : ?none:string -> 'a converter -> 'a option converter

  end

end
