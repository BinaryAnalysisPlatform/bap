open Core_kernel
open Regular.Std
open Bap.Std
open Format

include Self ()

open Bap_cache_types

module Filename = Caml.Filename
module Utils = Bap_cache_utils

module T = struct
  type t = config [@@deriving bin_io, compare, sexp]
end

include T

let (/) = Filename.concat

let version = 3

let default = {
  max_size = Int64.(4L * 1024L * 1024L * 1024L);
  overhead = 0.25;
  gc_enabled = true;
}

let lock_file = "lock"
let config_file = sprintf "config.%d" version
let cache_data  = "data"

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let dir_exists dir =
  Sys.file_exists dir && Sys.is_directory dir

let write path cfg =
  try Utils.unsafe_to_file (module T) path cfg
  with e ->
    warning "store config: %s" (Exn.to_string e)

let rec mkdir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then mkdir par;
  if not(Sys.file_exists path) then
    Unix.mkdir path 0o700

let init path =
  let data = path / cache_data in
  let conf = path / config_file in
  if not (dir_exists data)
  then mkdir data;
  if not (Sys.file_exists conf)
  then write conf default

let default_cache_dir = ref None

let set_cache_dir dir = default_cache_dir := Some dir

let cache_dir () =
  let cache_dir = match !default_cache_dir with
    | Some dir -> dir
    | None -> match getenv "XDG_CACHE_HOME" with
      | Some cache -> cache
      | None -> match getenv "HOME" with
        | None -> Filename.get_temp_dir_name () / "bap" / "cache"
        | Some home -> home / ".cache" / "bap" in
  cache_dir

let unsafe_init () = init @@ cache_dir ()

let config_path () = cache_dir () / config_file

let cache_data () = cache_dir () / cache_data

let lock_file () = cache_dir () / lock_file

let gc_threshold c =
  Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

let unsafe_write cfg =
  try Utils.unsafe_to_file (module T) (config_path ()) cfg
  with e ->
    warning "store config: %s" (Exn.to_string e)

let unsafe_read () =
  try Utils.unsafe_from_file (module T) @@ config_path ()
  with e ->
    warning "read config: %s" (Exn.to_string e);
    default
