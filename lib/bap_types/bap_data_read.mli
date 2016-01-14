open Core_kernel.Std
open Bap_data_types

type 'a t

(** A minimal complete definition is any method except
    [from_channel].

    If a class is defined only with [from_bigstring] or [from_bytes]
    then [from_channel] function will consume all input and pass it
    to the correspondings function.  *)
val create :
  ?of_channel     : (in_channel -> 'a) ->
  ?of_lexbuf      : (lexbuf -> 'a) ->
  ?of_scanbuf     : (scanbuf -> 'a) ->
  ?of_bigstring   : (bigstring -> 'a) ->
  ?of_bytes        : (bytes -> 'a) ->
  unit -> 'a t

val of_bytes : 'a t -> bytes -> 'a
val of_channel : 'a t -> in_channel -> 'a
val of_bigstring : 'a t -> bigstring -> 'a
