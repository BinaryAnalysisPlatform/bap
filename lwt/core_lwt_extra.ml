let catch t ~exn =  Lwt.catch t exn
let try_bind t ~ok ~exn = Lwt.try_bind t ok exn
let protect t ~finally = Lwt.finalize t finally
let don't_wait = Lwt.ignore_result
let failwith msg = Lwt.fail (Failure msg)
let failwithf fmt = Printf.ksprintf failwith fmt
let invalid_arg msg = Lwt.fail (Invalid_argument msg)
let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
