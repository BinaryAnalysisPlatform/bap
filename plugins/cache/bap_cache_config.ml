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

let config_file = sprintf "config.%d" version
let cache_data  = "data"

let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

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

let config_path () = cache_dir () / config_file
let cache_data  () = cache_dir () / cache_data

let gc_threshold c =
  Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

let write cfg =
  let path = config_path () in
  try Utils.to_file (module T) ~temp_dir:path path cfg
  with e ->
    warning "store config: %s" (Exn.to_string e)

let read () =
  try Utils.from_file (module T) @@ config_path ()
  with e ->
    warning "read config: %s" (Exn.to_string e);
    default
