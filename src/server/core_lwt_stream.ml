open Core_kernel.Std
open Core_lwt_container_intf
open Lwt


module Lwt_or_error = Core_lwt_or_error

include Lwt_stream


let wrap_push push =
  (); fun msg -> Or_error.try_with (fun () -> push msg)


let create () =
  let stream, push = create () in
  stream, wrap_push push

let create_with_reference () =
  let stream, push, refer = create_with_reference () in
  stream, wrap_push push, refer


let next s = Lwt_or_error.try_with (fun () -> next s)
let last_new s = Lwt_or_error.try_with (fun () -> last_new s)
let junk ?(n=1) s = njunk n s

let map ~f = map f
let map_s ~f = map_s f
let filter ~f = filter f
let filter_s ~f = filter_s f
let filter_map ~f = filter_map f
let filter_map_s ~f = filter_map_s f
let map_list ~f = map_list f
let map_list_s ~f = map_list_s f
let fold s ~f ~init = fold (fun x z -> f z x) s init
let fold_s s ~f ~init = fold_s (fun x z -> f z x) s init
let iter ~f = iter f
let iter_s ~f = iter_s f
let iter_p ~f = iter_p f

module Push_queue = struct
  type 'a t = 'a bounded_push
  let size q = q#size
  let resize q = q#resize
  let push q x =
    Lwt_or_error.try_with (fun () -> q#push x)

  let push_all q xs =
    Lwt_or_error.List.iter xs ~f:(push q)

  let close q = q#close
  let length q = q#size
  let blocked q = q#blocked
  let closed q = q#closed
  let set_reference (q : 'a t) (x : 'b) = q#set_reference x
end
