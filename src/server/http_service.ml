open Core_kernel.Std
open Core_lwt.Std
open Cohttp
open Lwt_log

let section = Section.make "Http_transport"
let max_messages = 1000

module Http = Cohttp_lwt_unix
module Body = Cohttp_lwt_body

let headers = Header.of_list [
    "allow", "GET, POST";
    "server", "BAP/0.1";
  ]


let start ~new_connection =
  let callback conn req body =
    let to_client,queue = Lwt.Stream.create_bounded max_messages in
    let of_client = Lwt.Stream.clone (Body.to_stream body) in
    new_connection (of_client, queue);
    let body = Body.of_stream to_client in
    Http.Server.respond ~flush:false ~headers ~status:`OK ~body () in
  let srv = Http.Server.make ~callback () in
  Http.Server.create srv >>= Lwt.Or_error.return


let () = Transport.register_service ~name:"http" ~start
