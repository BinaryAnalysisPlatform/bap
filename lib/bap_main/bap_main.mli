open Bap_future.Std

type error = ..
type ctxt

val init :
  ?features:string list ->
  ?library:string list ->
  ?argv:string array ->
  ?env:(string -> string option) ->
  ?log:[`Formatter of Format.formatter | `Dir of string] ->
  ?out:Format.formatter ->
  ?err:Format.formatter ->
  ?man:string ->
  ?name:string ->
  ?version:string ->
  ?default:(ctxt -> (unit,error) result) ->
  unit -> (unit, error) result

module Extension : sig
  type 'a typ

  val declare :
    ?requires:string list ->
    ?provides:string list ->
    ?doc:string ->
    ?name:string ->
    (ctxt -> (unit,error) result) -> unit

  val documentation : string -> unit

  module Command : sig
    type ('f,'r) t
    type 'a param

    val declare : ?doc:string -> string ->
      ('f,ctxt -> (unit,error) result) t -> 'f -> unit

    val args : ('a, 'a) t
    val ($) : ('a,'b -> 'c) t -> 'b param -> ('a,'c) t

    val argument :
      ?docv:string ->
      ?doc:string ->
      'a typ -> 'a param

    val arguments :
      ?docv:string ->
      ?doc:string ->
      'a typ -> 'a list param

    val switch :
      ?doc:('a -> string) ->
      ('a -> string) ->
      'a list -> 'a option param

    val switches :
      ?doc:('a -> string) ->
      ('a -> string) ->
      'a list -> 'a list param

    val dictionary :
      ?docv:string ->
      ?doc:('k -> string) ->
      ?short:('k -> char) ->
      ('k -> string) ->
      'k list ->
      'd typ -> ('k * 'd) list param

    val parameter :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      string ->
      'a typ -> 'a param

    val parameters :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      string ->
      'a typ -> 'a list param

    val flag :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string ->
      bool param

    val flags :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string ->
      int param

  end

  module Parameter : sig
    type 'a t

    val get : ctxt -> 'a t -> 'a

    val declare :
      ?as_flag:'a ->
      'a typ -> ?deprecated:string -> ?default:'a ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> 'a t

    val declare_list :
      ?as_flag:'a ->
      'a typ -> ?deprecated:string -> ?default:'a list ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->  string -> 'a list t

    val flag :
      ?deprecated:string ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->
      string -> bool t

    val determined : 'a t -> 'a future

    val doc_enum : ?quoted:bool -> (string * 'a) list -> string

    val version : string
    val datadir : string
    val libdir : string
    val confdir : string
  end

  module Context : sig
    type t = ctxt
    type info

    val plugins : ?features:string list -> ctxt -> info list
    val commands : ?features:string list -> ctxt -> info list

    val name : info -> string
    val doc : info -> string

    val digest : ?features:string list -> ctxt -> string
    val get : ctxt -> 'a Parameter.t -> 'a
    val pp : Format.formatter -> ctxt -> unit
  end


  module Syntax : sig
    val (-->) : ctxt -> 'a Parameter.t -> 'a
  end

  module Type : sig
    type 'a t = 'a typ

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


  module Error : sig
    type t = error = ..
    type t += Configuration
    type t += Invalid of string
    type t += Exit_requested of int
    type t += Unknown_plugin of string
    type t += Bug of exn * string

    val pp : Format.formatter -> t -> unit
    val register_printer : (t -> string option) -> unit
  end

end
