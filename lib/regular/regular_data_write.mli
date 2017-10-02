open Core_kernel.Std
open Regular_data_types
type 'a t

type bytes = Regular_bytes.t

val create :
  ?to_bytes  : ('a -> bytes) ->
  ?to_bigstring : ('a -> bigstring) ->
  ?dump  : (Out_channel.t -> 'a -> unit) ->
  ?pp : (Format.formatter -> 'a -> unit) ->
  ?size  : ('a -> int) ->
  ?blit_to_string:('a,string) copy ->
  ?blit_to_bigstring:('a,bigstring) copy ->
  unit -> 'a t

val size : 'a t -> 'a -> int
val to_channel : 'a t -> Out_channel.t -> 'a -> unit
val to_formatter : 'a t -> Format.formatter -> 'a -> unit
val to_bytes : 'a t -> 'a -> bytes
val to_bigstring : 'a t -> 'a -> bigstring
val blit_to_string : 'a t -> string -> 'a -> int -> unit
val blit_to_bigstring : 'a t -> bigstring -> 'a -> int -> unit
