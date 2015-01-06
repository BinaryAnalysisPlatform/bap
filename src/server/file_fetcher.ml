open Core_kernel.Std
open Core_lwt.Std


let fetch uri =
  match Uri.path uri with
  | "" -> Lwt.Or_error.errorf
            "Uri with a file scheme have empty path: %s"
            (Uri.to_string uri)
  | path ->
    Lwt.Or_error.try_with (fun () ->
        Lwt_unix.(openfile path [O_RDONLY] 0o400 >>| unix_file_descr))
    >>=? fun fd ->
    let data = Lwt_bytes.map_file ~fd ~shared:false () in
    Lwt.Or_error.return (Bigsubstring.create data)

let () = Transport.register_resource_fetcher ~scheme:"file" fetch
