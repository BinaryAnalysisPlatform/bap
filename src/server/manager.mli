(** Resource Manager.

*)
open Core_kernel.Std
open Core_lwt.Std
open Bap.Std

(** Abstract identifier.
    This types implements [Regular] interface see [Id]
    module. *)
type id

type 'a list1 = 'a * 'a list


(** Resource.

    Resources can have different data assosiated with
    it, depending on its nature.
*)
type ('mem,'img,'sec,'sym) res

(** Types of resource data  *)
type nil                        (** data not available  *)
type sym = Symbol.t             (** symbol  *)
type sec = Section.t            (** section  *)
type img = Image.t              (** image  *)

(** Unique Identifer  *)
module Id : Regular with type t = id


(** adds file image to the resources data base  *)
val add_image  : image -> id Lwt.Or_error.t

(** fetches memory chunk from a given [uri], and adds it to the
    resource pool *)
val add_memory : arch -> endian -> addr -> Uri.t -> id Lwt.Or_error.t

(** fetched image from a given [url], and stores it among other
    resources *)
val add_file : ?backend:string -> Uri.t -> id Lwt.Or_error.t

(** [string_of_id] is a synonym for [Id.to_string]  *)
val string_of_id : id -> string
val id_of_string : string -> id Or_error.t

val symbol_of_memory  : id -> id option
val section_of_symbol : id -> id option
val image_of_section  : id -> id option

val sections_of_image  : id -> id list
val symbols_of_section : id -> id list
val memory_of_symbol   : id -> id list

(** returns an empty list if it is not served *)
val links_of_id : id -> Uri.t list


(** Access to resource  *)

val fetch_memory  : ('a,_,_,_) res -> 'a Lwt.Or_error.t
val fetch_image   : (_,'a,_,_) res -> 'a Lwt.Or_error.t
val section       : (_,_,'a,_) res -> 'a
val symbol        : (_,_,_,'a) res -> 'a
val endian        : (_,_,_,_) res -> endian
val arch          : (_,_,_,_) res -> arch
val addr          : (_,_,_,_) res -> addr


(** Resource Visitor.
    Visitor is a function that accepts a resource and returns a value
    of type ['a Lwt.Or_error.t]
*)
type ('mem,'img,'sec,'sym,'a) visitor =
  ('mem,'img,'sec,'sym) res -> 'a Lwt.Or_error.t


(** Predefined visitors.

    This visitors are fully polymorphic over resource type, i.e., they
    can accept resource of any type. Most of the visitors do restrict
    you in a return type, e.g., if you're using [Return.none] visitor,
    then all you other visitors should also return ['a option]
*)
module Return : sig
  val unit   : (_,_,_,_,unit)     visitor
  val none   : (_,_,_,_,'a option) visitor
  val null   : (_,_,_,_,int)      visitor
  val nil    : (_,_,_,_,'a list)   visitor
  val error  : string -> 'a -> ('a -> Sexp.t) -> (_,_,_,_,_) visitor

  (** [errorf fmt args...] creates a visitor that will yield an error
      with a message created from format string [fmt] and arguments
      [args], e.g., [errorf "id %d has wrong type" id]
  *)
  val errorf : ('a, unit, string, (_,_,_,_,_) visitor) format4 -> 'a
end

(** retrieve resource with a specified [id] and apply corresponding
    function.
*)
val with_resource :
  chunk:(mem,nil,nil,nil,'a) visitor ->
  symbol:(mem list1, img,sec,sym,'a) visitor ->
  section:(mem,img,sec,nil,'a) visitor ->
  image:(mem list1,img,nil,nil,'a) visitor ->
  id -> 'a Lwt.Or_error.t
