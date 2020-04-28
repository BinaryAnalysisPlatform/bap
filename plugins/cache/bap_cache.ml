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
    capacity = 4 * 1024; (* 4 Gb  *)
    overhead = 0.25;
    gc_enabled = true;
  }

  let version = 3
  let config_file = sprintf "config.%d" version
  let cache_data  = "data"

  let config_file path = path // config_file
  let data  path = path // cache_data

  let gc_threshold c =
    c.capacity + Int.of_float (float c.capacity *. c.overhead)

  let default_root = ref None

  let set_root dir = default_root := Some dir

  let write path cfg =
    try Utils.binable_to_file (module T) path cfg
    with e ->
      warning "storing config: %s" (Exn.to_string e)

  let read path =
    try Utils.binable_from_file (module T) path
    with e ->
      warning "read config: %s" (Exn.to_string e);
      default

end

include Cfg

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let root () =
  let root = match !default_root with
    | Some dir -> dir // ".cache" // "bap"
    | None -> match getenv "XDG_CACHE_HOME" with
      | Some cache -> cache
      | None -> match getenv "HOME" with
        | None -> Filename.get_temp_dir_name () // "bap" // "cache"
        | Some home -> home // ".cache" // "bap" in
  root

let ensure_dir_exists path =
  try
    Unix.mkdir path 0o700
  with
  | Unix.(Unix_error (EEXIST,_,_)) -> ()
  | exn -> raise exn

let rec mkdir path =
  let par = Filename.dirname path in
  if not (Sys.file_exists par) then mkdir par;
  if not (Sys.file_exists path) then
    ensure_dir_exists path

let dir_exists dir = Sys.file_exists dir && Sys.is_directory dir
let _mkdir path = FileUtil.mkdir ~parent:true ~mode:(`Octal 0o700) path
let rmdir path = FileUtil.rm ~recurse:true [path]

let rename x y =
  Result.try_with (fun () -> Unix.rename x y)

(* todo: bad!! *)
let is_not_empty_error = function
  | Unix.(Unix_error (ENOTEMPTY,_,_))  -> true
  | _ -> false

let try_rename x y =
  try
    Unix.rename x y
  with
  | Unix.(Unix_error (ENOTEMPTY,_,_)) -> ()
  | exn -> raise exn

let with_temp_dir path ~f =
  let tmp = Filename.temp_file "tmp" "" in
  let tmp_dir = path // Filename.basename tmp in
  protect ~f:(fun () ->
      mkdir tmp_dir;
      f tmp_dir)
    ~finally:(fun () ->
        Sys.remove tmp;
        if dir_exists tmp_dir
        then rmdir tmp_dir)

let mkdir_from_tmp ~target ~f path =
  with_temp_dir path
    ~f:(fun tmp_dir ->
        f tmp_dir;
        try Unix.rename tmp_dir target
        with
        | Unix.(Unix_error (ENOTEMPTY,_,_)) -> ()
        | exn -> raise exn)

let init_cache_dir path =
  mkdir (Cfg.data path);
  Cfg.write (Cfg.config_file path) Cfg.default

let init_cache_dir () =
  let root = root () in
  let data = data root in
  if not (dir_exists data)
  then
    let parent = Filename.dirname root in
    mkdir_from_tmp ~target:root ~f:init_cache_dir parent

let config_file () = config_file @@ root ()
let data () = data @@ root ()
let write_config cfg = Cfg.write (config_file ()) cfg
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
      let idx = Utils.binable_from_file (module Compatibility.V2) index in
      Map.iteri idx.entries ~f:(fun ~key ~data:{path} ->
          rename path (dst // Data.Cache.Digest.to_string key))
    with e ->
      warning "can't read entries from index version 2: %s"
        (Exn.to_string e)

  let upgrade_from_index_v2 file =
    mkdir_from_tmp ~target:(data ())
      ~f:(upgrade_from_index_v2 file) (root ())

  let run () = match find_index () with
    | None -> ()
    | Some file ->
      begin
        match get_version file with
        | Ok 2 -> upgrade_from_index_v2 file;
        | _  -> warning "unknown index version"
      end;
      Sys.remove file
end

let size () =
  let path = data () in
  let size =
    Sys.readdir path |>
    Array.fold ~init:0L ~f:(fun s f ->
        try
          let file = path // f in
          Int64.(s + Unix.LargeFile.( (stat file).st_size ))
        with _ -> s) in
  Int64.(to_int_exn (size / 1024L / 1024L))

let init () =
  init_cache_dir ();
  Upgrade.run ()
