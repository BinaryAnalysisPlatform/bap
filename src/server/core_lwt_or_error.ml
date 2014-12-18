open Core_kernel.Std

module M = struct
  type 'a t = 'a Or_error.t Lwt.t
  let bind m f =
    Lwt.bind m (function
        | Ok r -> f r
        | Error _ as err -> Lwt.return err)
  let map m ~f = Lwt.map (fun r -> Result.map r ~f) m
  let map = `Custom map
  let return x = Lwt.return (Ok x)
end

type 'a t = 'a M.t

let fail err : _ t = Lwt.return (Error err)

let errorf fmt =
  Printf.ksprintf (fun msg -> Lwt.return (Or_error.error_string msg)) fmt

let error msg x sox : _ t =
  Lwt.return Or_error.(error msg x sox)

let error_string msg : _ t =
  Lwt.return Or_error.(error_string msg)

let unimplemented msg : _ t =
  Lwt.return Or_error.(unimplemented msg)

let combine_errors errs : 'a list t =
  let open Lwt in  (* in Async they use sequential map *)
  Lwt_list.map_p ident errs >>= fun errs ->
  return Or_error.(combine_errors errs)

let combine_errors_unit errs : unit t  =
  let open Lwt in
  combine_errors errs >>= function
  | Ok _ -> return (Ok ())
  | Error err -> Lwt.return (Error err)

let ok_unit = Lwt.return (Ok ())
let ok_true = Lwt.return (Ok true)
let ok_false = Lwt.return (Ok false)
let ok_nil = Lwt.return (Ok [])

let try_with ?(backtrace=false) f : 'a t =
  Lwt.try_bind f
    (fun r -> Lwt.return (Ok r))
    (fun exn ->
       let backtrace = if backtrace then Some `Get else None in
       fail (Error.of_exn ?backtrace exn))

let try_with_join ?backtrace (f : unit -> 'a t) : 'a t =
  let open Lwt in
  try_with ?backtrace f >>= fun err -> return (Or_error.join err)

module Sequence = Make_sequence(M)
module List = Make_list(M)
include Monad.Make(M)
end
