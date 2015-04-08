open Bap.Std

exception Bad_user_input

val byteweight : string -> Addr.Hash_set.t
val usersource : string -> Addr.Hash_set.t
val symbols : string -> Addr.Hash_set.t
val ida : string -> Addr.Hash_set.t
