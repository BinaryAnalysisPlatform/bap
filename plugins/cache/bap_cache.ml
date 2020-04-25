open Core_kernel
open Regular.Std
open Bap.Std
open Bap_cache_types

include Self ()

module Filename = Caml.Filename
module Utils = Bap_cache_utils

let (//) = Filename.concat

module Cfg = struct

  module T = struct
    type t = config [@@deriving bin_io, compare, sexp]
  end

  let default = {
    max_size = Int64.(4L * 1024L * 1024L * 1024L);
    overhead = 0.5;
    gc_enabled = true;
  }

  let version = 3
  let config_file = sprintf "config.%d" version
  let cache_data  = "data"

  let config_file path = path // config_file
  let data  path = path // cache_data

  let gc_threshold c =
    Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

  let default_root = ref None

  let set_root dir = default_root := Some dir

  let write path cfg =
    try Utils.to_file (module T) path cfg
    with e ->
      warning "storing config: %s" (Exn.to_string e)

  let read path =
    try Utils.from_file (module T) path
    with e ->
      warning "read config: %s" (Exn.to_string e);
      default

end

include Cfg

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let root () =
  let root = match !default_root with
    | Some dir -> dir
    | None -> match getenv "XDG_CACHE_HOME" with
      | Some cache -> cache
      | None -> match getenv "HOME" with
        | None -> Filename.get_temp_dir_name () // "bap" // "cache"
        | Some home -> home // ".cache" // "bap" in
  root

let dir_exists dir = Sys.file_exists dir && Sys.is_directory dir
let mkdir path = FileUtil.mkdir ~parent:true ~mode:(`Octal 0o700) path
let rmdir path = FileUtil.rm ~recurse:true [path]
let rename x y = Result.try_with (fun () -> Unix.rename x y)

let with_temp_dir parent ~f =
  let tmp = Filename.temp_file "tmp" "" in
  let tmp_dir = parent // tmp in
  protect ~f:(fun () ->
      mkdir tmp_dir;
      f tmp_dir)
    ~finally:(fun () ->
        Sys.remove tmp;
        if dir_exists tmp_dir then
          rmdir tmp_dir)

let init_cache_dir path =
  mkdir (Cfg.data path);
  Cfg.write (Cfg.config_file path) Cfg.default

let init_cache_dir () =
  let root = root () in
  if not (dir_exists root)
  then
    let parent = Filename.dirname root in
    with_temp_dir parent ~f:(fun tmp_root ->
        init_cache_dir tmp_root;
        match rename tmp_root root with
        | Ok () -> ()
        | Error (Unix.Unix_error (Unix.ENOTEMPTY,_,_)) ->
          (* ok, we inited in another process *)
          ()
        | Error exn ->
          error "can't init cache: %s\n" (Exn.to_string exn);
          raise exn)

let config_file () = config_file @@ root ()
let data () = data @@ root ()
let write_config = Cfg.write @@ config_file ()
let read_config () = Cfg.read @@ config_file ()

module Upgrade = struct

  let index_versions = [2;1]
  let index_file v = sprintf "index.%d" v
  let index_files = List.map index_versions ~f:index_file

  let find_index () =
    let files = List.map index_files ~f:(fun x -> root () // x) in
    List.find files ~f:Sys.file_exists

  let get_version path =
    let file = Filename.basename path in
    match String.chop_prefix file "index." with
    | None -> Ok 1
    | Some v ->
      try Ok (int_of_string v)
      with _ ->
        Error (Error.of_string (sprintf "unknown version %s" v))

  let upgrade_from_index_v2 index dst =
    let open Compatibility.V2 in
    let rename from to_ =
      Sys.rename from to_;
      Unix.chmod to_ 0o444 in
    try
      let idx = Utils.from_file (module Compatibility.V2) index in
      Map.iteri idx.entries ~f:(fun ~key ~data:{path} ->
          rename path (dst // Data.Cache.Digest.to_string key))
    with e ->
      warning "can't read entries from index version 2: %s"
        (Exn.to_string e)

  let upgrade_from_index_v2 file =
    with_temp_dir (root ()) ~f:(fun tmp_dir ->
        upgrade_from_index_v2 file tmp_dir;
        match rename tmp_dir (data ()) with
        | Ok () -> ()
        | Error (Unix.Unix_error (ENOTEMPTY,_,_)) ->
          (* ok, we upgraded index in another process *)
          ()
        | Error exn ->
          warning "can't read entries from index version 2: %s"
            (Exn.to_string exn))

  let run () = match find_index () with
    | None -> ()
    | Some file ->
      match get_version file with
      | Ok 2 ->
        upgrade_from_index_v2 file;
        Sys.remove file
      | Ok ver ->
        warning "can't read entries from index version %d" ver;
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

end

let size () =
  let path = data () in
  Sys.readdir path |>
  Array.fold ~init:0L ~f:(fun s f ->
      try
        let file = path // f in
        Int64.(s + Unix.LargeFile.( (stat file).st_size ))
      with _ -> s)

let init () =
  init_cache_dir ();
  Upgrade.run ()
