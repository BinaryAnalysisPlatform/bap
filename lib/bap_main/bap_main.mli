open Bap_future.Std

type error = ..

val init :
  ?features:string list ->
  ?library:string list ->
  ?argv:string array ->
  ?env:(string -> string option) ->
  ?log:[`Formatter of Format.formatter | `Dir of string] ->
  ?out:Format.formatter ->
  ?err:Format.formatter ->
  ?name:string ->
  ?version:string ->
  unit ->
  (unit, error) result

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
      print:('a -> string) -> 'a -> 'a t

    val bool : bool t
    val char : char t
    val int : int t
    val nativeint : nativeint t
    val int32 : int32 t
    val int64 : int64 t
    val float : float t
    val string : string t
    val enum : (string * 'a) list -> 'a t
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
    val doc_enum : ?quoted:bool -> (string * 'a) list -> string

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
      ?default:bool ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->
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

    val version : string
    val datadir : string
    val libdir : string
    val confdir : string
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
      'a Type.t -> 'a param

    val param :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      string ->
      'a Type.t -> 'a param

    val param_all :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      string ->
      'a Type.t -> 'a list param

    val flag :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string ->
      bool param
  end

  module Syntax : sig
    val (-->) : Config.ctxt -> 'a Config.param -> 'a
  end
end
