open Core_kernel
open Regular.Std
open Bap.Std
open Format

include Self ()

open Bap_cache_types

module Filename = Caml.Filename
module Utils = Bap_cache_utils

let (/) = Filename.concat

let version = 3

let default = {
  max_size = Int64.(4L * 1024L * 1024L * 1024L);
  overhead = 0.25;
  gc_enabled = true;
}

let config_file = sprintf "config.%d" version

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let write file cfg =
  try Utils.to_file (module T) file cfg
  with e -> warning "store config: %s" (Exn.to_string e)

let rec init_dir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then init_dir par;
  if not(Sys.file_exists path) then
    let () = Unix.mkdir path 0o700 in
    write (path / config_file) default

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
  init_dir cache_dir;
  cache_dir

let config_path () = cache_dir () / config_file

let read () =
  try Utils.from_file (module T) @@ config_path ()
  with e ->
    warning "read config: %s" (Exn.to_string e);
    default

let write cfg = write (config_path ()) cfg

let gc_threshold c =
  Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))
