open Core_kernel.Std

module Std : sig

  exception Not_a_bundle
  type bundle

  (** [main_bundle ()] returns a program's bundle if the program is
      bundled, otherwise creates a fresh new bundle in current working
      directory. The name of the bundle is a basename of
      [Sys.executable_name] with a [bundle] extension.  *)
  val main_bundle : unit -> bundle

  (**/**)

  (* used by a program loader to set a main bundle of currently
     executing application.  *)
  val set_main_bundle : bundle -> unit

  (**/**)


  module Manifest : sig
    type t = {
      name : string;
      version : string;
      desc : string;
      main : string;
      author : string;
      date : float;
      requires : string list;
      provides : string list;
      url : string option;
      license : string option;
      copyrights : string option;
    } with bin_io, compare, fields, sexp

    val create :
      ?author:string ->
      ?version:string ->
      ?main:string ->
      ?date:float ->
      ?desc:string ->
      ?requires:string list ->
      ?provides:string list ->
      ?url:string ->
      ?license:string ->
      ?copyrights:string -> string -> t

    include Stringable with type t := t

    val pp : Format.formatter -> t -> unit

  end

  type manifest = Manifest.t

  module Bundle : sig
    type t = bundle

    (** creates new bundle or opens existing  *)
    val of_uri : Uri.t -> t

    val manifest : t -> manifest

    val get_file : ?name:string -> t -> Uri.t -> Uri.t option
    val get_data : t -> string -> string option

    val list : t -> string list

    val update : t -> f:(string -> [
        | `Drop
        | `Copy
        | `Proc of (string -> unit)
        | `Map  of (string -> string)
      ]) -> unit

    val update_manifest : t -> f:(manifest -> manifest) -> unit

    val insert_files : t -> (string option * Uri.t) list -> unit

    val insert_file : ?name:string -> t -> Uri.t -> unit

    val insert_data : t -> name:string -> data:string -> unit

    module Builder : sig
      type t
      val create : unit -> t
      val put_file : ?name:string -> t -> Uri.t -> unit
      val put_data : t -> name:string -> data:string -> unit
      val embed_manifest : t -> manifest -> unit
      val flush : ?sign:bool -> t -> Uri.t -> unit
    end
  end
end
