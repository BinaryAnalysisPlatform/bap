open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

module Chan = Lwt_zmq.Socket

let section = Section.make "Zmq_client"

let max_connections = 16

let ok = Lwt.Or_error.return
let err fmt = Lwt.Or_error.errorf fmt
let fail exn = Lwt.Or_error.fail (Error.of_exn exn)

let host_of_uri uri = match Uri.scheme uri, Uri.host uri with
  | Some "zmq+tcp", Some host -> ok ("tcp://"^host)
  | Some "zmq+ipc", None -> ok ("ipc://" ^ Uri.path uri)
  | Some "zmq+tcp", None -> err "zmq+tcp scheme expects a host"
  | Some "zmq+ipc", Some _ -> err "zmq_ipc shouldn't have a host"
  | Some scheme, _ -> err "wrong scheme: '%s'" scheme
  | None, _ -> err "expected scheme"

let query_of_uri uri =
  match Uri.get_query_param uri "chunk" with
  | Some query -> ok query
  | None -> err "url '%s' should contain 'chunk' query" @@
    Uri.to_string uri

let noraise f a =
  try Lwt.Or_error.return (f a)
  with exn -> fail exn

let connect uri sock =
  host_of_uri uri >>=?
  noraise (ZMQ.Socket.connect sock)

let send_request uri chan () =
  query_of_uri uri >>=? fun q ->
  Lwt.catch (fun () -> Chan.send chan q >>= ok) ~exn:fail

let data_of_chunks chunks =
  let len = List.fold chunks ~init:0
      ~f:(fun sum s -> sum + String.length s) in
  match len with
  | 0 -> err "empty chunk"
  | len ->
    let dst = Bigstring.create len in
    let _n  : int = List.fold chunks ~init:0 ~f:(fun pos src ->
        let len = String.length src in
        Bigstring.From_string.blit
          ~src ~src_pos:0
          ~dst ~dst_pos:pos ~len;
        pos+len) in
    Bigsubstring.of_bigstring dst |> ok

let recv chan () : Bigsubstring.t Lwt.Or_error.t =
  Lwt.try_bind (fun () -> Chan.recv_all chan)
    ~ok:data_of_chunks ~exn:fail

let disconnect uri sock =
  host_of_uri uri >>=?
  noraise (ZMQ.Socket.disconnect sock)

let fetch uri sock =
  let chan = Chan.of_socket sock in
  connect uri sock >>=?
  send_request uri chan >>=?
  recv chan >>=? fun data ->
  disconnect uri sock >>=?
  fun () -> ok data


let main () =
  let ctxt = ZMQ.Context.create () in
  let socks = Lwt_pool.create max_connections
      (fun () -> Lwt.return ZMQ.Socket.(create ctxt req)) in
  List.iter ["zmq+tcp"; "zmq+ipc"] ~f:(fun scheme ->
      Transport.register_fetcher ~scheme
        (fun uri -> Lwt_pool.use socks (fetch uri)) |> function
      | `Ok -> ()
      | `Duplicate ->
        ign_warning_f ~section "Failed to register ZMQ client\
                                for scheme %s, as it is already\
                                registered." scheme)


let () = main ()
