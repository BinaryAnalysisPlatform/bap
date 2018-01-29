open Core_kernel.Std
open Bap.Std

type t

type 'a relocations

val create : Ogre.doc -> t Or_error.t

val internals : t -> addr relocations

val externals : t -> string relocations

val to_seq : 'a relocations -> (addr * 'a) seq

val find : addr -> addr -> 'a relocations -> 'a option
