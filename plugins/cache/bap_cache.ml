open Core_kernel
open Regular.Std
open Bap.Std
open Bap_cache_types

include Self ()

module Filename = Caml.Filename
module Utils = Bap_cache_utils
module Cfg = Bap_cache_config

let (//) = Filename.concat

let cache_dir = Cfg.cache_dir
let cache_data = Cfg.cache_data

module Upgrade = struct

  let index_versions = [2;1]
  let index_file v = sprintf "index.%d" v
  let index_files = List.map index_versions ~f:index_file

  let find_index () =
    let files = List.map index_files ~f:(fun x -> cache_dir () // x) in
    List.find files ~f:Sys.file_exists

  let get_version path =
    let file = Filename.basename path in
    match String.chop_prefix file "index." with
    | None -> Ok 1
    | Some v ->
      try Ok (int_of_string v)
      with _ ->
        Error (Error.of_string (sprintf "unknown version %s" v))

  let rename from to_ =
    Sys.rename from to_;
    Unix.chmod to_ 0o444

  let upgrade_from_index_v2 file =
    let open Compatibility.V2 in
    try
      let idx = Utils.from_file (module Compatibility.V2) file in
      let dir = cache_data () in
      Map.iteri idx.entries ~f:(fun ~key ~data:{path} ->
          rename path (dir // Data.Cache.Digest.to_string key))
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

  let from_index x = My_bench.with_args1 from_index x "from-index"

end

let with_lock ~f lock =
  let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
  Unix.lockf lock Unix.F_LOCK 0;
  protect ~f
    ~finally:(fun () -> Unix.(lockf lock F_ULOCK 0; close lock))

let size () =
  let path = cache_data () in
  Sys.readdir path |>
  Array.fold ~init:0L ~f:(fun s f ->
      try
        let file = path // f in
        Int64.(s + Unix.LargeFile.( (stat file).st_size ))
      with _ -> s)

let dir_exists dir =
  Sys.file_exists dir && Sys.is_directory dir

let rec mkdir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then mkdir par;
  if not(Sys.file_exists path) then
    Unix.mkdir path 0o700

let init_cache_dir () =
  let data = Cfg.cache_data () in
  let conf = Cfg.config_path () in
  if not (dir_exists data)
  then mkdir data;
  if not (Sys.file_exists conf)
  then Cfg.(write default)

let init () =
  let lock = "bap_cache_init.lock" in
  let lock = Filename.get_temp_dir_name () // lock in
  with_lock lock ~f:(fun () ->
      init_cache_dir ();
      Upgrade.from_index ())


let init x = My_bench.with_args1 init x "init"
