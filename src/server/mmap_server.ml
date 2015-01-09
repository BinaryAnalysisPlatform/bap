open Core_kernel.Std
open Core_lwt.Std
open Lwt_log

let section = Section.make "mmap_server"


(* the data field should be mmaped otherwise we're risking to run out
   fd resources. If a user provides us with a data that is mapped,
   but doesn't give us a filename of the mapped file, then we won't
   save the data in our bag, and issue a debug warning
*)

type file = {
  data : Bigstring.t;
  path : string;
}

type files = file Bag.t

let scheme = "mmap"


let url_of_file ?query sub path : Uri.t =
  let length = Bigsubstring.length sub in
  let offset = Bigsubstring.pos sub in
  let url = Uri.make ~scheme ~path ~query:[
      "length", [Int.to_string length];
      "offset", [Int.to_string offset];
    ] () in
  match query with
  | None -> url
  | Some v -> Uri.with_query' url ["q", v]

let save_to_file data : string =
  let path = Filename.temp_file "bap_" ".mmap" in
  let fd = Unix.(openfile path [Unix.O_RDWR] 0o600) in
  let size = Bigstring.length data in
  let dst = Lwt_bytes.map_file ~fd ~size ~shared:true () in
  Bigstring.blito ~src:data ~dst ();
  Bigstring.unsafe_destroy dst;
  Unix.close fd;
  path

let add_and_forget files file =
  let _ : file Bag.Elt.t = Bag.add files file in
  ()

let main () =
  let files : files = Bag.create () in
  let create ?query ?file data =
    match file with
    | Some file -> Lwt.Or_error.return (url_of_file ?query data file)
    | None ->
      let base = Bigsubstring.base data in
      match Bag.find_elt files ~f:(fun f -> phys_equal f.data base) with
      | Some file -> let path = Bag.Elt.(value file).path in
        Lwt.Or_error.return (url_of_file ?query data path)
      | None ->
        let path = save_to_file base in
        if Bigstring.is_mmapped base then
          ign_debug ~section
            "user provided a mmaped file without a name"
        else
          add_and_forget files {data=base; path};
        Lwt.Or_error.return (url_of_file ?query data path) in
  Transport.register_resource_server ~scheme ~create

let () = main ()
