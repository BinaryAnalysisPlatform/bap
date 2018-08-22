open Format
open Core_kernel.Std
open Bap_future.Std
open Regular.Std
open Bap_types.Std

type 'a param
type error

val run :
  ?options:(string * string) list ->
  ?argv:string array ->
  ?input:[`Data of Bigstring.t | `Path of string] ->
  unit ->
  (unit,error) result

module Param : sig
  val values : 'a param -> 'a stream
  val digest : 'a param -> 'a -> digest
  val current : 'a param -> 'a
end

module Create() : sig
  val name : string
  val version : string
  val doc : string
  val argv : string array

  val debug   : ('a,formatter,unit) format -> 'a
  val info    : ('a,formatter,unit) format -> 'a
  val warning : ('a,formatter,unit) format -> 'a
  val error   : ('a,formatter,unit) format -> 'a

  val debug_formatter : formatter
  val info_formatter : formatter
  val warning_formatter : formatter
  val error_formatter : formatter

  val report_progress :
    ?task:string ->
    ?note:string ->
    ?stage:int ->
    ?total:int -> unit -> unit

  module Config : sig
    val version : string
    val datadir : string
    val libdir : string
    val confdir : string

    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type nonrec 'a param = 'a param
    type 'a converter

    val input : [
      | `Path of string
      | `Data of Bigstring.t
    ] param

    val converter :
      ?digest:('a -> digest) ->
      'a parser -> 'a printer -> 'a -> 'a converter

    val deprecated : string

    val param :
      'a converter -> ?deprecated:string -> ?default:'a -> ?as_flag:'a ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> 'a param

    val param_all :
      'a converter -> ?deprecated:string -> ?default:'a list ->
      ?as_flag:'a -> ?docv:string -> ?doc:string ->
      ?synonyms:string list ->  string -> 'a list param

    val flag :
      ?deprecated:string ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> bool param

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
    val doc_enum : ?quoted:bool -> (string * 'a) list -> string
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
