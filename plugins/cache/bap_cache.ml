open Core_kernel
open Regular.Std
open Bap.Std
open Format

include Self ()

module Filename = Caml.Filename

open Bap_cache_types

module Utils = Bap_cache_utils
module Config = Bap_cache_config

let (/) = Filename.concat

let cache_dir = Config.cache_dir

let mtime () =
  let s = Unix.stat @@ cache_dir () in
  Unix.(s.st_mtime)

let entries () = Sys.readdir @@ cache_dir ()

let size () =
  let dir = cache_dir () in
  entries () |>
  Array.fold ~init:0L ~f:(fun size e ->
      let s = Unix.stat (dir / e) in
      Int64.(size + of_int Unix.(s.st_size)))

let size x = My_bench.with_args1 size x "size"

module GC = struct

  let () = Random.self_init ()

  let remove_entry path =
    try Sys.remove path
    with exn ->
      warning "unable to remove entry: %s" (Exn.to_string exn)

  let entry_size e =
    let s = Unix.stat e in
    Unix.(s.st_size)

  let remove_entries size_to_free entries =
    let dir = cache_dir () in
    let cfg = Config.config_file in
    let rec loop entries freed n =
      if freed < size_to_free && n <> 0 then
        let elt = Random.int n in
        if entries.(elt) = cfg then loop entries freed n
        else
          let () = Array.swap entries (Random.int n) (n - 1) in
          let path = dir / entries.(n - 1) in
          let size = entry_size path in
          remove_entry path;
          loop entries Int64.(freed + of_int size) (n - 1) in
    loop entries 0L (Array.length entries)

  let remove size =
    entries () |> remove_entries size

  let remove sz = My_bench.with_args1 remove sz "remove"

  let remove_all () =
    let dir = cache_dir () in
    let cfg = Config.config_file in
    Array.iter (entries ()) ~f:(fun e ->
        if e <> cfg then remove_entry @@ dir / e)

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

  let upgrade_from_index_2 file =
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
      Config.(write default);
      match get_version file with
      | Ok 2   ->
        upgrade_from_index_2 file;
        Sys.remove file
      | Ok ver -> warning "can't read entries from index version %d" ver
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

  let run = from_index

end

let upgrade = Upgrade.run
