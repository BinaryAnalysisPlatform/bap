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

  (* For internal usage only.

     Used by a program loader to set a main bundle of currently
     executing application.
  *)
  val set_main_bundle : bundle -> unit

  (**/**)



  (** Program meta infromation.  *)
  module Manifest : sig
    type t = {
      name : string;            (** program name *)
      version : string;         (** program version *)
      desc : string;            (** one line description *)
      main : string;            (** entry point *)
      author : string;          (** program author *)
      date : float;             (** last update date *)
      requires : string list;   (** required libraries *)
      provides : string list;   (** provided features *)
      url : string option;      (** project URL *)
      license : string option;  (** project license *)
      copyrights : string option; (** copyright holders *)
      tags : string list;       (** bundle tags  *)
    } [@@deriving bin_io, compare, fields, sexp]

    (** [create name] create a bundle for a program with a given [name] *)
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
      ?copyrights:string ->
      ?tags:string list -> string -> t

    include Stringable with type t := t
  end

  type manifest = Manifest.t


  (** Program Bundle.

      Bundle is a collection of data associated with a program. To
      access the bundle, use the {!main_bundle} function, e.g.,

      {[
        open Bap_bundle.Std

        let bundle = main_bundle ()
      ]}

  *)
  module Bundle : sig
    type t = bundle

    (** creates new bundle or opens existing  *)
    val of_uri : Uri.t -> t


    (** [manifest bundle] extracts program manifest from the [bundle] *)
    val manifest : t -> manifest


    (** [get_file ?name bundle uri] extracts a file.

        Extracts a file specified by a [uri] from a [bundle] and returns
        a uri pointing to the extracted file, if was found. The
        optional parameter [name] allows to specify the desired
        filename for the extraction.  *)
    val get_file : ?name:string -> t -> Uri.t -> Uri.t option


    (** [get_data bundle path] extracts data specified by a [path] as
        a string.  *)
    val get_data : t -> string -> string option


    (** [list bundle] returns a list of paths, that are accessible in
        the [bundle]. *)
    val list : t -> string list

    (** [update_manifest bundle ~f] update program manifest with
        function [f].  *)
    val update_manifest : t -> f:(manifest -> manifest) -> unit


    (** [insert_files bundle spec] bundle files.

        The [spec] is a list of pairs, where the first constituent of
        a pair is a desired path of the file in the bundle, and the
        second constituent is the uri of the file, that should be
        inserted. If the first element of the pair is [None], then the
        file will be inserted under the same path, as it was in the
        file system.
    *)
    val insert_files : t -> (string option * Uri.t) list -> unit


    (** [insert_file ?name bundle uri] insert a file specified by the
        [uri]. If [name] is specified, then the file will be stored
        under the specified [name] in the bundle.*)
    val insert_file : ?name:string -> t -> Uri.t -> unit


    (** [insert_data bundle ~name ~data] insert [data] at path [name].  *)
    val insert_data : t -> name:string -> data:string -> unit

    (** [update bundle ~f:action] add, remove or update data in the
        bundle.

        This is a swiss-knife function, that can do arbitrary bundle
        modification. See {!get_file}, {!get_data}, {!insert_files},
        {!insert_file} and {!insert_data} for an easier to use
        interface.


        The [action] function is called on each path,
        and must return one of the following:

        - [`Drop] - to remove the path from the bundle;
        - [`Copy] - to keep it untouched;
        - [`Proc f] - extract it, process the file with a function
                      [f], and put back, where function [f] accepts
                      a temporary name of extracted file;
        - [`Map f] - map the contents of the file with function [f].

        Warning. Modification of a bundle, associated with an
        installed plugin or application will lead to an undefined
        behavior. The function is intended for building a new
        bundle. Once it is created it may be sealed and made
        readonly.*)
    val update : t -> f:(string -> [
        | `Drop
        | `Copy
        | `Proc of (string -> unit)
        | `Map  of (string -> string)
      ]) -> unit



    (** Incremental bundle builder.

        Using this module it is possible to build bundle
        recipe incrementally. The data, that was added to
        the bundle will not be copied until the [flush] method is
        called. *)
    module Builder : sig
      type t


      (** [create ()] creates a builder.  *)
      val create : unit -> t


      (** [put_file ?name builder uri] insert a file specified by the
          [uri]. If [name] is specified, then the file will be stored
          under the specified [name] in the bundle.*)
      val put_file : ?name:string -> t -> Uri.t -> unit


      (** [put_data builder ~name ~data] insert [data] at path
          [name].  *)
      val put_data : t -> name:string -> data:string -> unit


      (** [embed_manifest builder manifest] embeds a manifest. If it
          was already embedded, then old one will be overwritten.  *)
      val embed_manifest : t -> manifest -> unit


      (** [flush builder output] finish the building and output the
          resulting bundle into the file [output].  *)
      val flush : t -> Uri.t -> unit
    end
  end
end
