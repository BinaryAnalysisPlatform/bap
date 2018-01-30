open Core_kernel.Std
open Bap.Std

type t

type 'a fixups

val create : Ogre.doc -> t Or_error.t
val relocations : t -> addr fixups
val externals   : t -> string fixups
val find : from:addr -> to_:addr -> 'a fixups -> 'a option
