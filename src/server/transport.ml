open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

let section = Section.make "Transport"

type server = {
  uri : Uri.t;
  close : unit -> unit sexp_opaque;
} with sexp_of

type data = Bigsubstring.t

type provider =
  ?query:string -> ?file:string -> data -> server Lwt.Or_error.t

type fetcher = Uri.t -> data Lwt.Or_error.t


type t = {
  served : server Lwt_sequence.t;
  providers : provider String.Table.t;
  fetchers  : fetcher String.Table.t;
}  with fields

let t = {
  served = Lwt_sequence.create ();
  providers = String.Table.create ();
  fetchers  = String.Table.create ();
}



let log level fmt err =
  log_f ~level ~section (fmt ^^ ": %s") Error.(to_string_hum err)



let serve ?query ?file data =
  let providers = String.Table.data t.providers in
  Lwt.List.map providers ~f:(fun create -> create ?query ?file data)
  >>= fun r ->
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
    Lwt.Or_error.return (s,ss)

let fetch uri =
  match Uri.scheme uri with
  | None -> Lwt.Or_error.errorf "url '%s' doesn't contain scheme"
              Uri.(to_string uri)
  | Some scheme -> match String.Table.find t.fetchers scheme with
    | None ->
      Lwt.Or_error.errorf "Don't know how to fetch '%s' scheme" scheme
    | Some fetch -> fetch uri

let register_fetcher ~scheme fetcher =
  String.Table.add t.fetchers ~key:scheme  ~data:fetcher


let register_server ~scheme ~create =
  String.Table.add t.providers ~key:scheme  ~data:create
