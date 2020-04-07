open Core_kernel
open Regular.Std
open Bap.Std
open Format

include Self ()

open Cache_types

module Filename = Caml.Filename

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

let index_version = 3
let all_versions = [3;2;1]
let index_file = sprintf "index.%d" index_version
let lock_file = "lock"
let versions =
  List.map all_versions ~f:(sprintf "index.%d")

let default_config = {
  max_size = 4_000_000_000L;
  overhead = 0.25;
  gc_enabled = true;
}

let empty = {
  config = default_config;
  entries = Data.Cache.Digest.Map.empty;
  current_size  = 0L;
}

let index_file () = cache_dir () / index_file

module IO = struct

  let from_file : type t.
    (module Binable.S with type t = t) -> string -> t = fun b file ->
    let module T = (val b) in
    let fd = Unix.(openfile file [O_RDONLY] 0o400) in
    try
      let data = Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout false [|-1|] in
      let pos_ref = ref 0 in
      let t = T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref in
      Unix.close fd;
      t
    with e -> Unix.close fd; raise e
  [@@warning "-D"]

  let open_temp () =
    let tmp =
      Filename.temp_file ~temp_dir:(cache_dir ()) "tmp" "index" in
    try tmp, Unix.(openfile tmp [O_RDWR] 0o600)
    with e -> Sys.remove tmp; raise e

  let to_file : type t.
    (module Binable.S with type t = t) -> string -> t -> unit =
    fun b file data ->
    let module T = (val b) in
    let tmp,fd = open_temp () in
    let size = T.bin_size_t data in
    let () =
      try
        let buf =
          Mmap.V1.map_file
            fd Bigarray.char Bigarray.c_layout true [|size|] in
        let _ = T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0
            data in
        Unix.close fd
      with e -> Unix.close fd; Sys.remove tmp; raise e in
    Sys.rename tmp file
  [@@warning "-D"]

end

let read t file =
  try Some (IO.from_file t file)
  with e ->
    warning "read index: %s" (Exn.to_string e);
    None

let write index =
  try IO.to_file (module T) (index_file ()) index
  with e -> warning "store index: %s" (Exn.to_string e)

let with_lock ~f =
  let lock = cache_dir () / lock_file in
  let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
  Unix.lockf lock Unix.F_LOCK 0;
  protect ~f
    ~finally:(fun () ->
        Unix.(lockf lock F_ULOCK 0; close lock))

module Upgrade = struct

  let size entries =
    Map.fold entries ~init:0L ~f:(fun ~key:_ ~data:e size ->
        Int64.(size + e.size))

  let upgrade_index old_file version =
    let () = match version with
      | 1 ->
        warning "can't load index, version 1 is not supported anymore";
        write empty;
      | 2 ->
        let index =
          match read (module Compatibility.V2) old_file with
          | None -> empty
          | Some index ->
            let entries = Map.map index.entries
                ~f:(fun e -> { path = e.path; size = e.size }) in
            let current_size = size entries in {
              config = default_config;
              current_size;
              entries;
            } in
        write index
      | x ->
        warning
          "can't update index version from %d to %d" x index_version in
    Sys.remove old_file

  let find_index () =
    let files = List.map versions ~f:(fun x -> cache_dir () / x) in
    List.find files ~f:Sys.file_exists

  let get_version path =
    let file = Filename.basename path in
    match String.chop_prefix file "index." with
    | None -> Ok 1
    | Some v ->
      try Ok (int_of_string v)
      with _ ->
        Error (Error.of_string (sprintf "unknown version %s" v))

  let run ()  =
    match find_index () with
    | None -> ()
    | Some file -> match get_version file with
      | Ok ver when Int.(ver = index_version) -> ()
      | Ok ver -> upgrade_index file ver
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

end

let upgrade = Upgrade.run

let read () =
  match read (module T) (index_file ()) with
  | None -> empty
  | Some idx -> idx
