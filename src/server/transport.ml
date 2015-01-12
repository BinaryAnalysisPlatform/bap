open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

let section = Section.make "Transport"

type data = Bigsubstring.t
type ('a,'b) pipe = 'a Lwt.Stream.t * ('b Lwt.Stream.bounded_push)
type 'a list1 = 'a List1.t


type provider =
  ?query:string -> ?file:string -> data -> Uri.t Lwt.Or_error.t

type fetcher = Uri.t -> data Lwt.Or_error.t

type connection = (string,string) pipe
type service = new_connection:(connection -> unit) -> unit Lwt.Or_error.t


type t = {
  served : Uri.t Lwt_sequence.t;
  providers : provider String.Table.t;
  fetchers  : fetcher String.Table.t;
  services  : service String.Table.t;
} with fields

open Fields


let t = {
  served = Lwt_sequence.create ();
  providers = String.Table.create ();
  fetchers  = String.Table.create ();
  services  = String.Table.create ();
}

let log level fmt err =
  log_f ~level ~section (fmt ^^ ": %s") Error.(to_string_hum err)


let combine r =
  let servers,failures =
    List.partition_map r
      ~f:(function Ok s -> `Fst s | Error e -> `Snd e ) in
  match servers,failures with
  | [],[] -> Lwt.Or_error.errorf "No providers are registered"
  | [],errs ->
    Error.(Error.tag (of_list errs) "All providers failed") |>
    Lwt.Or_error.fail
  | s::ss, errs ->
    Lwt.List.iter errs ~f:(log Warning "provider failed") >>= fun () ->
    List1.create s ss |>
    Lwt.Or_error.return


let serve_resource ?query ?file data =
  String.Table.data t.providers |>
  Lwt.List.map ~how:`Parallel  ~f:(fun create -> create ?query ?file data)
  >>= combine

let fetch_resource uri =
  match Uri.scheme uri with
  | None -> Lwt.Or_error.errorf "url '%s' doesn't contain scheme"
              Uri.(to_string uri)
  | Some scheme -> match String.Table.find t.fetchers scheme with
    | None ->
      Lwt.Or_error.errorf "Don't know how to fetch '%s' scheme" scheme
    | Some fetch -> fetch uri

let register what ~key ~data =
  String.Table.add (Fieldslib.Field.get what t) ~key ~data |> function
  | `Ok -> ()
  | `Duplicate ->
    ign_warning_f ~section "Can register %s in %s: Duplicate entry."
      key (Fieldslib.Field.name what)

let register_resource_fetcher ~scheme fetcher =
  register fetchers ~key:scheme ~data:fetcher

let register_resource_server ~scheme ~create =
  register providers ~key:scheme ~data:create

let register_service ~name ~start =
  register services ~key:name ~data:start

let start_service ?name ~new_connection () =
  match name with
  | None ->
    String.Table.data t.services |>
    Lwt.List.map ~how:`Parallel ~f:(fun start -> start ~new_connection)
    >>= combine >>=? fun (results : unit list1) ->
    notice "All services finished" >>=
    Lwt.Or_error.return
  | Some name -> match String.Table.find t.services name with
    | Some start -> start ~new_connection
    | None -> Lwt.Or_error.errorf "Unknown service protocol: %s" name

let registered_fetchers : string list =
  String.Table.keys t.fetchers
