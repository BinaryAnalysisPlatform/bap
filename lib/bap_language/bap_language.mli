open Core_kernel.Std

(** A programming language abstraction.

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

    val create : name:string -> 'a t

    val name : 'a t -> string

    module type Property = sig
      type t
      type 'a data

      val create : unit -> t
      val set : t -> 'a language -> 'a data -> unit
      val get : t -> 'a language -> 'a data option
    end

    module Property(T : T1) : Property with type 'a data = 'a T.t
  end
end
