open Core_kernel.Std
open Core_lwt.Std
open Lwt_log
open Bap.Std
open Rpc

let section = Lwt_log.Section.make "bap_server"

let stub name = Lwt.Or_error.unimplemented name

let init ver = stub "init"
let load_file ?loader uri = stub "load_file"
let load_chunk addr arch endian uri = stub "chunk"
let get_insns  stop_on res_id = stub "get_insns"
let get_resource res_id = stub "get_resource"

let accept (req : request) : Response.msg Lwt.Or_error.t =
  Request.accept req ~init ~load_file ~load_chunk
    ~get_insns ~get_resource |> Lwt.return |> Lwt.Or_error.join

exception Stopped

let run_exn (request, reply) : unit Lwt.t =
  let reply x = reply x >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
      error_f "Service is finished with error: %s"
        (Error.to_string_mach err) >>= fun () ->
      Lwt.fail Stopped in
  let handle_request req =
    Request.id req |> Lwt.return >>=? fun id ->
    accept req >>|? Response.create id in
  Lwt.Stream.iter_s request ~f:(fun req ->
      handle_request req >>= function
      | Ok msg -> reply msg
      | Error err -> match Request.id req with
        | Ok id ->
          let str = Error.to_string_hum err in
          let msg = Response.error `Warning str in
          Response.create id msg |> reply
        | Error err' ->
          let err = Error.of_list [err; err'] in
          let msg = Error.to_string_hum err in
          warning_f ~section "Ignoring junk request: %s" msg)


let run (pipe : (request, response) Transport.pipe) : unit Lwt.Or_error.t =
  Lwt.catch (fun () -> run_exn pipe >>= fun () -> Lwt.Or_error.return ())
    ~exn:Lwt.Or_error.of_exn
