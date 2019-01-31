open Core_kernel
open Regular.Std
open Bap.Std

(** A high level programming language.

    Sometimes it is useful to know in which high-level language the
    program was originally written. Based on this knowledge it is
    possible to recover some abstractions from the binary and make the
    analysis more precise. This abstraction allows to write analysis,
    that are dependent on the particular language. This module is also
    a place, where language detection algorithms meet and decide in
    which language the program was written.

    {2 Writing an analysis that depends on the language}

    A common way to write an analysis that depends on the language is
    demonstrated in the following example, that will add an automatic
    a hypothetical pass that resolves [applyN] function calls into
    regular calls:

    {[

      open Language.Std
      open OCaml.Std

      let resolve_apply proj = (* do something with proj *) proj

      let () = Future.upon Language.chosen @@ fun lang ->
        match Language.name lang with
        | OCaml ->
          Project.register_pass ~autorun:true ~f:resolve_apply
        | _ -> ()
    ]}

    The example, assumes that there exists a library that defines
    module [OCaml.Std], that extends the set of language names with
    constructor OCaml, e.g.,
    {[
      module Std : sig
        type Language.name += OCaml

        val ocaml : language
        module OCaml : sig
          type t = ocaml
          (* implementation specific to the OCaml language *)
        end
      end
    ]}


    {2 Detecting the language}

    First of all there should be only one system component, that is
    responsible for selecting the language. In the Platform it is the
    [language] platform the provides a user interface for choosing the
    language. The library by itself doesn't decide the language, it is
    only the common ground, where different plugins meet. So, if you
    wrote an analysis that detects program language, then you need to


    A ['a language] is high level language represented with abstraction
    ['a]. It is possible to associate properties that depends on the type
    of abstraction with the language.

    For example, given an information written in language ['a lang] and
    represented with type ['a] it is possible to have a registry of
    passes, that takes this abstraction and transforms projects, or
    a set of frontend that takes a file, and returns a parsed
    representation, and so on.

    Example, lets assume that with every language there is a pass:

    {[module Passes = Language.Property(struct
          type 'a t = 'a -> project -> project
        end)]}

    and that every language may have some frontend (i.e., a parser):

    {[module Frontends = Language.Property(struct
          type 'a t = string -> 'a
        end)]}

    Then given a language [lang] it is possible to write a generic
    function:

    {[

      let frontends = Frontends.create ()
      let passes = Passes.create ()

      let pass lang input proj =
        match Frontends.get frontends lang with
        | None -> proj
        | Some parse -> match Passes.get passes lang with
          | None -> proj
          | Some pass -> pass (parse input) proj
    ]}


    Where [frontends] and [passes] are static tables, that can be
    dynamically populated at runtime by plugins, that add support for
    new languages and transformations based on this languages.

    A more concrete example (and more useful) is the ABI
    framework, that maps API languages to ABI implementations.
*)


module Std : sig

  type 'a language

  module Language : sig
    type 'a t = 'a language

    val create : name:string -> 'a language
    val name : 'a language -> string

    module Property(T : T1) : sig
      type t
      type 'a data = 'a T.t

      val create : unit -> t
      val set : t -> 'a language -> 'a data -> unit
      val get : t -> 'a language -> 'a data option
    end
  end
end
