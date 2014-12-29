(** Provides transporting

    Hides underlying transporting protocols

*)
open Core_kernel.Std
open Core_lwt.Std

type ('a,'b) pipe = 'a Lwt.Stream.t * ('b -> unit Lwt.Or_error.t)
type 'a list1 = 'a List1.t

(** returns a pipe for each registered service provider  *)
val start : ?name:string -> unit -> (string,string) pipe list1 Lwt.Or_error.t

val serve_resource : ?query:string -> ?file:string ->
  Bigsubstring.t -> (Uri.t list1) Lwt.Or_error.t

val fetch_resource : Uri.t -> Bigsubstring.t Lwt.Or_error.t

val register_service :
  name:string ->
  start:(unit -> (string,string) pipe Lwt.Or_error.t) -> unit

val register_resource_fetcher :
  scheme:string ->
  (Uri.t -> Bigsubstring.t Lwt.Or_error.t) -> unit

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

val register_resource_server :
  scheme:string ->
  create:(?query:string ->
          ?file: string ->
          Bigsubstring.t -> Uri.t Lwt.Or_error.t) -> unit
