open Bap_future.Std

type error = ..

val init :
  ?logdir:string ->
  ?etcdir:string ->
  ?argv:string array ->
  ?command:string ->
  unit -> (unit,error) result

module Extension : sig

  module Error : sig
    type t = error = ..
    type t += Configuration
    type t += Invalid of string
  end

  module Type : sig
    type 'a t

    val define :
      parse:(string -> 'a) ->
      print:('a -> string) -> 'a t

    val bool : bool t
    val char : char t
    val int : int t
    val nativeint : nativeint t
    val int32 : int32 t
    val int64 : int64 t
    val float : float t
    val string : string t
    val enum : (string * 'a) list -> 'a t
    val doc_enum : ?quoted:bool -> (string * 'a) list -> string
    val file : string t
    val dir : string t
    val non_dir_file : string t
    val list : ?sep:char -> 'a t -> 'a list t
    val array : ?sep:char -> 'a t -> 'a array t
    val pair : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t
    val t2 : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t
    val t3 : ?sep:char -> 'a t -> 'b t -> 'c t ->
      ('a * 'b * 'c) t
    val t4 : ?sep:char -> 'a t -> 'b t -> 'c t ->
      'd t -> ('a * 'b * 'c * 'd) t
    val some : ?none:string -> 'a t -> 'a option t
  end

  module Config : sig
    type 'a param

    type reader = {get : 'a. 'a param -> 'a}
    [@@deprecated "use ctxt instead"]

    type ctxt = reader [@@warning "-D"]

    val get : ctxt -> 'a param -> 'a

    val deprecated : string

    val param :
      'a Type.t -> ?deprecated:string -> ?default:'a -> ?as_flag:'a ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> 'a param

    val param_all :
      'a Type.t -> ?deprecated:string -> ?default:'a list ->
      ?as_flag:'a -> ?docv:string -> ?doc:string ->
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

    val version : string param
    val datadir : string param
    val libdir : string param
    val confdir : string param
  end

  module Command : sig
    type ('f,'r) t
    type 'a param

    val declare : ?doc:string -> string ->
      ('f,unit) t -> (Config.ctxt -> 'f) -> unit

    val ($) : ('a,'b -> 'c) t -> 'b param -> ('a,'c) t
    val args : 'a param -> ('a -> 'b, 'b) t
    val rest : 'a param -> 'a list param

    val argument :
      ?docv:string ->
      ?doc:string ->
      'a Type.t ->
      string param

    val param :
      'a Type.t ->
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      ?as_flag:'a ->
      string -> 'a param

    val param_all :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string -> 'a Type.t ->
      'a list param

    val flag :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string -> bool param
  end

  module Syntax : sig
    val (-->) : Config.ctxt -> 'a Config.param -> 'a
  end
end
