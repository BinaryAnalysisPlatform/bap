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
let cache_data = Cfg.cache_data

let with_lock' ~f lock =
  let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
  Unix.lockf lock Unix.F_LOCK 0;
  protect ~f
    ~finally:(fun () -> Unix.(lockf lock F_ULOCK 0; close lock))

let with_lock ~f =
  let lock = Cfg.lock_file () in
  with_lock' ~f lock

let file_size x = Unix.LargeFile.( (stat x).st_size )

let fold_entries ~f ~init =
  let dir = cache_data () in
  Sys.readdir dir |>
  Array.fold ~init ~f:(fun acc e -> f acc @@ dir/e )

let unsafe_size () =
  fold_entries ~init:0L ~f:(fun sz e -> Int64.(sz + file_size e))

let with_size ~f = with_lock ~f:(fun () -> f @@ unsafe_size ())

module GC = struct

  let () = Random.self_init ()

  let remove path =
    try Sys.remove path
    with exn ->
      warning "unable to remove entry: %s" (Exn.to_string exn)

  let remove_random dir size_to_free entries =
    let entries_number = Array.length entries in
    let rec loop freed_size removed =
      if removed < entries_number && freed_size < size_to_free
      then
        let len = entries_number - removed in
        let elt = Random.int len in
        let () = Array.swap entries elt (len - 1) in
        let path = dir / entries.(len - 1) in
        let size = file_size path in
        remove path;
        loop Int64.(freed_size + size) (removed + 1) in
    loop 0L 0

  let clean () =
    with_lock ~f:(fun () ->
        fold_entries ~init:() ~f:(fun _ -> remove))

  let shrink ?threshold ~upto () =
    let open Int64 in
    with_size ~f:(fun size ->
        let upper_bound = match threshold with
          | None -> upto
          | Some t -> t in
        if size > upper_bound then
          let dir = cache_data () in
          remove_random dir (size - upto) (Sys.readdir dir))

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
      let idx = Utils.unsafe_from_file (module Compatibility.V2) file in
      let dir = cache_data () in
      Map.iteri idx.entries ~f:(fun ~key ~data:{path} ->
          Sys.rename path @@ dir / Data.Cache.Digest.to_string key)
    with e ->
      warning "can't read entries from index version 2: %s"
        (Exn.to_string e)

  let from_index () = match find_index () with
    | None -> ()
    | Some file ->
      Cfg.(unsafe_write default);
      match get_version file with
      | Ok 2 ->
        upgrade_from_index_v2 file;
        Sys.remove file
      | Ok ver ->
        warning "can't read entries from index version %d" ver;
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

end

let size () = with_size ~f:ident
let read_config () = with_lock ~f:Cfg.unsafe_read
let write_config cfg = with_lock ~f:(fun () -> Cfg.unsafe_write cfg)

let init () =
  let lock = "bap_cache_init.lock" in
  let lock = Filename.get_temp_dir_name () / lock in
  with_lock' lock ~f:(fun () ->
      Cfg.unsafe_init ();
      Upgrade.from_index ())
