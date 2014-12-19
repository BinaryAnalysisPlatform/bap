(** Provides transporting

    Hides underlying transporting protocols

*)
open Core_kernel.Std
open Core_lwt.Std

type server = {
  uri : Uri.t;
  close : unit -> unit
} with sexp_of


val serve : ?query:string -> ?file:string ->
  Bigsubstring.t -> (server * server list) Lwt.Or_error.t

val fetch : Uri.t -> Bigsubstring.t Lwt.Or_error.t

val register_fetcher :
  scheme:string ->
  (Uri.t -> Bigsubstring.t Lwt.Or_error.t) -> [`Ok | `Duplicate]

(** [register ~scheme ~create] a service provider.
    Each time the resource with the specified [scheme] is going to be
    served the provided function will be called with two parameters:

    [query] a suggested query string, to distinguish the served
    resource from others. The [query] string is only a hint and it may
    even doesn't required to be unique, so that it is completely a
    server responsibility to provide a unique uri for the given data.
    Also ther query is not expected to be properly encoded.

    [file] a name of a local file that is mapped to
    [Bigsubstring.base]. The service provider may rely on the fact,
    that if the filename provided, then it is mapped.

*)

val register_server :
  scheme:string ->
  create:(?query:string ->
          ?file: string ->
          Bigsubstring.t -> server Lwt.Or_error.t) -> [`Ok | `Duplicate]
