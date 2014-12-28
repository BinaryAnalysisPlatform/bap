open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

module Channel = Lwt_zmq.Socket

let section = Section.make "Zmq_server"

(* We split big blobs of data into smaller chunks *)
let max_chunk_size = 1 lsl 20


(** data that is served  *)
type chunks = string list String.Table.t


(* the fun part with 0MQ is that we can start from simple REQ/REP
   pattern and then extend it with DEALER/PROXY/ROUTER pattern
   transparently. *)

(** server socket *)
type socket = {
  chan : [`Rep] Lwt_zmq.Socket.t ; (** comm channel         *)
  uri  : Uri.t;                    (** url that we serve    *)
  scheme : string;                 (** scheme that we serve *)
}

type supported_transport = [`tcp | `ipc]
with enumerate, sexp

let endpoint_of_proto = function
  | `tcp -> "tcp://*:*"
  | `ipc -> "ipc://*"

(* for tcp transport we need to substitute local specicific
   address, like [0.0.0.0] with externally visible address.
*)
let prepare_endpoint_for_export proto endpoint = match proto with
  | `ipc -> endpoint
  | `tcp ->  (* this assumes that host is properly configured *)
    let host = Unix.gethostname () in
    Uri.with_host endpoint (Some host)

let string_of_proto = function
  | `tcp -> "tcp"
  | `ipc -> "ipc"

let create_socket context proto : socket =
  let socket = ZMQ.Socket.(create context rep)  in
  let endpoint = endpoint_of_proto proto in
  ZMQ.Socket.bind socket endpoint;
  let endpoint = ZMQ.Socket.get_last_endpoint socket in
  let uri = Uri.of_string endpoint in
  let scheme = Uri.scheme uri |> Option.map ~f:(sprintf "zmq+%s") in
  let uri = Uri.with_scheme uri scheme |>
            prepare_endpoint_for_export proto in
  let chan = Channel.of_socket socket in
  {chan; uri; scheme = uw scheme}


(* REQ/REP sockets should interleave requests and replies, and two
   requests or two replies in a row will provoke an EFSM error. The
   problem is that we cannot know for sure the current state of the
   socket in a case of a failure, e.g., if we have failed on waiting
   for request, do we still in a request state, or we moved somehow to
   the reply state?

   The idea to recover is always send empty message in a case of
   error.  If we already replied, then nothing will happen and we will
   remain in the same state, i.e. the process_request will proceed
   normally, otherwise, we will send an empty message, indicating that
   something goes wrong, and proceed to process_request. *)
let serve (chunks : chunks) sock : unit =
  let chan = sock.chan in
  let process_request () =
    Channel.recv chan >>= fun req ->
    match String.Table.find chunks req with
    | None -> Lwt.failwithf "Bad query '%s'" req
    | Some chunks -> Channel.send_all chan chunks in
  let reply_nothing () =
    Lwt.catch (fun () -> Channel.send chan "")
      ~exn:(fun exn -> warning ~exn ~section "Failing on resync") in
  let rec run () =
    Lwt.try_bind process_request ~ok:run ~exn:(fun exn ->
        warning ~exn ~section "Failed to serve request" >>=
        reply_nothing >>= run) in
  Lwt.async run

let insert_chunk next table (key : string) (data) : string =
  let rec loop key =
    match String.Table.add table ~key ~data with
    | `Ok -> key
    | `Duplicate -> loop (key ^ next ()) in
  loop key

let chunk_of_data data : string list =
  let pos  = Bigsubstring.pos data in
  let len  = Bigsubstring.length data in
  let data = Bigsubstring.base data in
  let size = min len max_chunk_size in
  let n = size + len - 1 / size in
  List.init n ~f:(fun i ->
      let src_pos = pos + i * size in
      let left = pos + len - src_pos in
      let size = min size left in
      let buff = String.create size in
      Bigstring.To_string.blit ~len:size
        ~src:data ~src_pos
        ~dst:buff ~dst_pos:0;
      buff)

let main () =
  let id = ref 0L in
  let next () = Int64.incr id; Int64.to_string !id in
  let context = ZMQ.Context.create () in
  let chunks : chunks = String.Table.create () in
  let insert_chunk = insert_chunk next chunks in
  let sockets = all_of_supported_transport |>
                List.map ~f:(create_socket context) in
  List.iter sockets ~f:(serve chunks);

  let create uri ?query ?file data =
    let chunk = chunk_of_data data in
    let query = Option.value query ~default:"" in
    let query = insert_chunk query chunk in
    Lwt.Or_error.return (Uri.with_query' uri  ["chunk", query]) in


  List.iter sockets ~f:(fun socket ->
      Transport.register_resource_server
        ~scheme:socket.scheme
        ~create:(create socket.uri))

let () = main ()
