open Core_kernel.Std
open Core_lwt.Std
open Lwt_log
open Bap.Std

let section = Lwt_log.Section.make "bap_server"

let handle_session (of_client,to_client) =
  let to_client res =
    to_client (Rpc.Response.to_string res) in
  let of_client = Lwt.Stream.filter_map of_client ~f:(fun msg ->
      match Rpc.Request.of_string msg with
      | Ok req -> Some req
      | Error err ->
        ign_warning_f ~section "Failed with %s, when parsing '%s'"
          (Error.to_string_hum err) msg;
        None) in
  Server.run (of_client,to_client) >>= function
  | Ok () -> return ()
  | Error err -> error_f ~section "Interaction failed: %s"
                   (Error.to_string_hum err)


let main () =
  let new_connection connection =
    Lwt.async (fun () -> handle_session connection) in
  Transport.start_service ~new_connection () >>= function
  | Ok () -> return ()
  | Error err ->
    error_f ~section "Failed to start server: %s"
      Error.(to_string_hum err)

let () =
  let module H = Http_service in
  Lwt.Main.run @@ main ()
