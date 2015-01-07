(** Resource Manager.

*)
open Core_kernel.Std
open Core_lwt.Std
open Bap.Std

(** Abstract identifier.
    This types implements [Regular] interface see [Id]
    module. *)
type id

type 'a list1 = 'a List1.t


(** Resource.

    Resources can have different data assosiated with
    it, depending on its nature.
*)
type ('mem,'img,'sec,'sym) res

(** Types of resource data  *)
type nil                        (** data not available  *)
type mem
type sym = Symbol.t             (** symbol  *)
type sec = Section.t            (** section  *)
type img

(** Unique Identifer  *)
module Id : Regular with type t = id


(** adds file image to the resources data base  *)
val add_image  : ?file:string -> image -> id Lwt.Or_error.t

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

val images : id list
val sections : id list
val symbols : id list
val chunks : id list

(** Access to resource  *)

val memory  : ('a,_,_,_) res -> 'a
val image   : (_,'a,_,_) res -> 'a
val section : (_,_,'a,_) res -> 'a
val symbol  : (_,_,_,'a) res -> 'a
val endian  : (_,_,_,_) res -> endian
val arch    : (_,_,_,_) res -> arch
val addr    : (_,_,_,_) res -> addr
val links   : (_,_,_,_) res -> Uri.t list1
val id      : (_,_,_,_) res -> id

val links_of_memory : mem -> Uri.t list1
val links_of_image  : img -> Uri.t list1
val fetch_memory    : mem -> Memory.t Lwt.Or_error.t
val fetch_image     : img -> Image.t  Lwt.Or_error.t

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
  image:(nil,img,nil,nil,'a) visitor ->
  id -> 'a Lwt.Or_error.t
