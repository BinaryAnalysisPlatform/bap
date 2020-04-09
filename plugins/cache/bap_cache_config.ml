open Core_kernel
open Regular.Std
open Bap.Std
open Format

include Self ()

open Bap_cache_types

module Filename = Caml.Filename
module Utils = Bap_cache_utils

let (/) = Filename.concat

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let rec mkdir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then mkdir par;
  if not(Sys.file_exists path) then
    Unix.mkdir path 0o700

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
  mkdir cache_dir;
  cache_dir

let version = 3

let default = {
  max_size = 4_000_000_000L;
  overhead = 0.25;
  gc_enabled = true;
}

let config_file = sprintf "config.%d" version

let config_path () = cache_dir () / config_file

let read () =
  try Utils.from_file (module T) @@ config_path ()
  with e ->
    warning "read config: %s" (Exn.to_string e);
    default

let write cfg =
  try Utils.to_file (module T) (config_path ()) cfg
  with e -> warning "store config: %s" (Exn.to_string e)
