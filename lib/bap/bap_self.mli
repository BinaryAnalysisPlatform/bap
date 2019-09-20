open Format
open Core_kernel
open Bap_future.Std
open Bap_main.Extension


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

    type 'a param = 'a Config.param

    type 'a converter = 'a Type.t

    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type reader = {get : 'a. 'a param -> 'a}
    type ctxt = reader

    val converter : 'a parser -> 'a printer -> 'a -> 'a Type.t

    val deprecated : string

    val get : ctxt -> 'a param -> 'a

    val param :
      ?as_flag:'a ->
      'a Type.t -> ?deprecated:string -> ?default:'a ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> 'a param

    val param_all :
      ?as_flag:'a ->
      'a Type.t -> ?deprecated:string -> ?default:'a list ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->  string -> 'a list param

    val flag :
      ?deprecated:string ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> bool param

    val determined : 'a param -> 'a future

    val when_ready : (ctxt -> unit) -> unit

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
