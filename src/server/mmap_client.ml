open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

let section = Section.make "mmap_client"
let scheme = "mmap"

let err fmt = Lwt.Or_error.errorf fmt

type entry = {
  path : string;
  data : Bigstring.t;
}

let empty = {
  path = "";
  data = Bigstring.create 0;
}

let entry path = { empty with path}

module Weak_set = struct
  include Caml.Weak.Make(struct
      type t = entry
      let equal t1 t2  =
        String.equal t1.path t2.path
      let hash t = String.hash t.path
    end)
  let find_exn = find
  let find t elt =
    try Some (find_exn t elt) with Not_found -> None
end

let int_of_string str =
  try Some (Int.of_string str) with exn -> None

let int_of_param uri name =
  let str = Uri.to_string uri in
  match Uri.get_query_param uri name with
  | None -> err "uri '%s' doesn't contain %s" str name
  | Some off -> match int_of_string off with
    | None -> err "%s value '%s' is not a valid integer" name off
    | Some n -> Lwt.Or_error.return n

let substring_of_entry uri entry  =
  int_of_param uri "length" >>=? fun len ->
  int_of_param uri "offset" >>|? fun pos ->
  Bigsubstring.create ~len ~pos entry.data

let openfile path =
  Lwt.Or_error.try_with (fun () ->
      Lwt_unix.(openfile path [O_RDONLY] 0o400 >>| unix_file_descr))

let main () =
  let files = Weak_set.create 256 in
  let fetch uri =
    let str = Uri.to_string uri in
    match Uri.path uri with
    | "" -> err "url '%s' has an empty path" str
    | path -> match Weak_set.find files (entry path) with
      | Some entry -> substring_of_entry uri entry
      | None ->
        openfile path >>=? fun fd ->
        let data = Lwt_bytes.map_file ~fd ~shared:false () in
        let entry = {data; path} in
        Weak_set.add files entry;
        substring_of_entry uri entry in
  Transport.register_resource_fetcher ~scheme fetch

let () = main ()
