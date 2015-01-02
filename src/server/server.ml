open Core_kernel.Std
open Core_lwt.Std
open Lwt_log
open Bap.Std
open Rpc

module Res = Manager

let section = Lwt_log.Section.make "bap_server"

let stub name = Lwt.Or_error.unimplemented name

let init ver = stub "init"
let load_file ?loader uri = stub "load_file"
let load_chunk addr arch endian uri = stub "chunk"
let get_insns  stop_on res_id = stub "get_insns"

let get_resource res_id =
  let unimplemented r = Res.Return.errorf "unimplemented" r in
  let get_mem mem : 'a Rpc.resource Lwt.Or_error.t =
    Res.fetch_memory mem >>|? fun m ->
    Res.links_of_memory mem, m in
  Lwt.return @@ Res.id_of_string res_id >>=? fun id ->
  Res.with_resource id
    ~chunk:(fun r -> get_mem (Res.memory r) >>|? Response.memory)
    ~symbol:(fun r ->
        let sym = Res.symbol r in
        let mem = Res.memory r in
        let m,ms = List1.hd mem, List1.tl mem |> List1.to_list in
        get_mem m >>=? fun m ->
        Lwt.Or_error.List.map ms ~how:`Parallel ~f:get_mem >>|? fun ms ->
        Response.symbol sym (List1.create m ms))
    ~image:(fun r ->
        let img = Res.image r in
        let img_id = Res.id r in
        let links = Res.links_of_image img in
        let secs = Res.sections_of_image img_id |>
                   List.map ~f:Res.string_of_id in
        Res.fetch_image img >>|? Tuple2.create links >>|?
        Response.image ~secs)
    ~section:(fun r ->
        let sec = Res.section r in
        let sec_id = Res.id r in
        let syms = Res.symbols_of_section sec_id |>
                   List.map ~f:Res.string_of_id  in
        let mem = Res.memory r in
        get_mem mem >>|? Response.section ~syms sec)



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
