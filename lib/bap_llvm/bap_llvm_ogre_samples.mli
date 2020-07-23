

module Symbols(Fact : Ogre.S) : sig
  val symbols : unit Fact.t
end

module Sections(Fact : Ogre.S) : sig
  val sections : unit Fact.t
end

module Relocatable_symbols(Fact : Ogre.S) : sig
  val symbols : unit Fact.t
end

module Relocatable_sections(Fact : Ogre.S) : sig
  val sections : unit Fact.t
end

module Code_regions(Fact : Ogre.S) : sig
  val code_regions : unit Fact.t
end
