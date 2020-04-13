open Core_kernel
open Regular.Std
open Bap.Std
open Bap_cache_types

include Self ()

module Filename = Caml.Filename
module Utils = Bap_cache_utils
module Cfg = Bap_cache_config

let (/) = Filename.concat

let cache_dir = Cfg.cache_dir
let lock_file = "lock"

let read () = Sys.readdir @@ cache_dir ()

let size () =
  let (+) = Int64.(+) in
  let dir = cache_dir () in
  read () |>
  Array.fold ~init:0L
    ~f:(fun sz e -> sz + Unix.LargeFile.( (stat @@ dir / e).st_size ))

let with_lock ~f =
  let lock = cache_dir () / lock_file in
  let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
  Unix.lockf lock Unix.F_LOCK 0;
  protect ~f
    ~finally:(fun () -> Unix.(lockf lock F_ULOCK 0; close lock))


module GC = struct

  let () = Random.self_init ()

  let remove_entry path =
    try Sys.remove path
    with exn ->
      warning "unable to remove entry: %s" (Exn.to_string exn)

  let file_size x = Unix.LargeFile.( (stat x).st_size )

  let remove_entries is_entry min_length size_to_free =
    let dir = cache_dir () in
    let rec loop entries freed len =
      if freed < size_to_free && len > min_length then
        let elt = Random.int len in
        if is_entry @@ entries.(elt) then
          let last = len - 1 in
          let () = Array.swap entries elt last in
          let path = dir / entries.(last) in
          let size = file_size path in
          remove_entry path;
          loop entries Int64.(freed + size) last
        else loop entries freed len in
    let files = read () in
    loop files 0L (Array.length files)

  let remove size =
    let dir = cache_dir () in
    let protected = [dir / Cfg.config_file; dir / lock_file] in
    let is_entry = Fn.non @@ List.mem ~equal:String.equal protected in
    remove_entries is_entry (List.length protected) size

  let clean () =
    with_lock ~f:(fun () ->
        let dir = cache_dir () in
        Array.iter (read ()) ~f:(fun e ->
            if e <> Cfg.config_file then remove_entry @@ dir / e))

  let with_size ~f = with_lock ~f:(fun () -> f @@ size ())

  let shrink ?threshold ~upto () =
    let open Int64 in
    with_size ~f:(fun size ->
        let upper_bound = match threshold with
          | None -> upto
          | Some t -> t in
        if size > upper_bound then
          remove (size - upto))

  let size () = with_size ~f:ident

end

module Upgrade = struct

  let index_versions = [2;1]
  let index_file v = sprintf "index.%d" v
  let index_files = List.map index_versions ~f:index_file

  let find_index () =
    let files = List.map index_files ~f:(fun x -> cache_dir () / x) in
    List.find files ~f:Sys.file_exists

  let get_version path =
    let file = Filename.basename path in
    match String.chop_prefix file "index." with
    | None -> Ok 1
    | Some v ->
      try Ok (int_of_string v)
      with _ ->
        Error (Error.of_string (sprintf "unknown version %s" v))

  let upgrade_from_index_v2 file =
    let open Compatibility.V2 in
    try
      let idx = Utils.from_file (module Compatibility.V2) file in
      let cache_dir = cache_dir () in
      Map.iteri idx.entries ~f:(fun ~key ~data:{path} ->
          Sys.rename path @@ cache_dir / Data.Cache.Digest.to_string key)
    with e ->
      warning "can't read entries from index version 2: %s"
        (Exn.to_string e)

  let from_index () = match find_index () with
    | None -> ()
    | Some file ->
      Cfg.(write default);
      match get_version file with
      | Ok 2 ->
        upgrade_from_index_v2 file;
        Sys.remove file
      | Ok ver ->
        warning "can't read entries from index version %d" ver;
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

  let run () = with_lock ~f:from_index

end

let upgrade = Upgrade.run
