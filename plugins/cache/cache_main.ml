open Core_kernel.Std
open Regular.Std
open Bap.Std
open Format
include Self()

type entry = {
  atime   : float;
  ctime   : float;
  hits    : int;
  path    : string;
  size    : int64;
} [@@deriving bin_io, compare, sexp]

type config = {
  max_size : int64;
} [@@deriving bin_io, compare, sexp]

type index = {
  config  : config;
  entries : entry Data.Cache.Digest.Map.t;
} [@@deriving bin_io, compare, sexp]

let (/) = Filename.concat

module Index_file = struct
  let index_version = 2
  let index_file = sprintf "index.%d" index_version

  let is_index path =
    String.is_prefix ~prefix:"index" (Filename.basename path)

  let get_version path =
    let file = Filename.basename path in
    match String.chop_prefix file "index." with
    | None -> Ok 1
    | Some v ->
      try Ok (int_of_string v)
      with _ ->
        Error (Error.of_string (sprintf "unknown version %s" v))
end

module Index = struct
  include Index_file

  let lock_file = "lock"
  let default_config = {
    max_size = 5_000_000_000L;
  }
  let empty = {
    config = default_config;
    entries = Data.Cache.Digest.Map.empty;
  }
  let perm = 0o770
  let getenv opt = try Some (Sys.getenv opt) with Not_found -> None

  let rec mkdir path =
    let par = Filename.dirname path in
    if not(Sys.file_exists par) then mkdir par;
    if not(Sys.file_exists path) then
      Unix.mkdir path perm

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

  let size idx =
    Map.fold idx.entries ~init:0L ~f:(fun ~key:_ ~data:e size ->
        Int64.(size + e.size))

  (* freq is a frequence of accesses to the cache entry in
     access per second. *)
  let freq e =
    Float.(of_int e.hits / (e.atime - e.ctime))

  let evict_entry idx =
    Map.to_sequence idx.entries |>
    Seq.min_elt ~cmp:(fun (_,e1) (_,e2) ->
        Float.compare (freq e1) (freq e2))
    |> function
    | None -> idx
    | Some (e,_) -> {idx with entries = Map.remove idx.entries e}

  let rec clean idx =
    let size = size idx in
    if size > 0L && size > idx.config.max_size
    then clean (evict_entry idx)
    else idx

  let remove_entry e =
    try Sys.remove e.path
    with exn ->
      warning "unable to remove entry: %s" (Exn.to_string exn)

  let remove_files old_index new_index =
    Map.iteri old_index.entries ~f:(fun ~key ~data:e ->
        if not (Map.mem new_index.entries key)
        then remove_entry e)

  module T = struct
    type t = index [@@deriving bin_io]
  end

  let from_file : type t.
    (module Binable.S with type t = t) -> string -> t = fun b file ->
    let module T = (val b) in
    let fd = Unix.(openfile file [O_RDONLY] 0o400) in
    try
      let data = Bigstring.map_file ~shared:false fd (-1) in
      let pos_ref = ref 0 in
      let t = T.bin_read_t data ~pos_ref in
      Unix.close fd;
      t
    with e -> Unix.close fd; raise e

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
          let buf = Bigstring.map_file ~shared:true fd size in
          let _ = T.bin_write_t buf ~pos:0 data in
          Unix.close fd
        with e -> Unix.close fd; Sys.remove tmp; raise e in
      Sys.rename tmp file

  let index_of_file file =
    try from_file (module T) file
    with e ->
      warning "read index: %s" (Exn.to_string e);
      empty

  let index_to_file file index =
    try to_file (module T) file index
    with e -> warning "store index: %s" (Exn.to_string e)

  let with_index ~f =
    let cache_dir = cache_dir () in
    let file = cache_dir / index_file in
    let lock = cache_dir / lock_file in
    let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
    Unix.lockf lock Unix.F_LOCK 0;
    protect ~f:(fun () ->
        let init = index_of_file file in
        let index',data = f cache_dir init in
        remove_files init index';
        let index = clean index' in
        remove_files index' index;
        index_to_file file index;
        data)
      ~finally:(fun () ->
          Unix.(lockf lock F_ULOCK 0; close lock))

  let update ~f = with_index ~f:(fun dir idx -> f dir idx,())

  let run ~f = with_index ~f:(fun _dir idx -> idx, f idx)

  let update_entry idx src entry = {
    idx with
    entries = Map.change idx.entries src (fun _ -> entry)
  }

  let with_entry src ~f =
    with_index ~f:(fun dir idx ->
        match Map.find idx.entries src with
        | None -> idx,None
        | Some entry ->
          let entry,res = f entry in
          update_entry idx src entry,res)

  let upgrade_old_index file version =
    let () = match version with
      | 1 ->
        let old =
          try Sexp.load_sexp file |> index_of_sexp
          with exn ->
            warning "can't load index: %s" (Exn.to_string exn);
            empty in
        index_to_file (cache_dir () / index_file) old;
      | x ->
        warning
          "can't update index version from %d to %d" x index_version in
    Sys.remove file

  let upgrade () =
    FileUtil.ls (cache_dir ()) |>
    List.find ~f:is_index |> function
    | None -> ()
    | Some file -> match get_version file with
      | Ok ver when Int.(ver = index_version) -> ()
      | Ok ver when Int.(ver < index_version) -> upgrade_old_index file ver
      | Ok ver ->
        error "unsupported index version %d, current is %d" ver index_version
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)
end

let size file =
  Unix.LargeFile.((stat file).st_size)

let cleanup () =
  Index.update ~f:(fun _ idx ->
      {idx with entries = Data.Cache.Digest.Map.empty});
  exit 0

let set_size size =
  Index.update ~f:(fun _ idx ->
      {idx with config = {max_size = Int64.(size * 1024L * 1024L)}});
  exit 0

let print_info () =
  Index.run ~f:(fun idx ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb idx.config.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb (Index.size idx));
  exit 0

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Index.set_cache_dir dir

let create reader writer =
  let save id proj =
    Index.update ~f:(fun cache_dir index ->
        let ctime = Unix.time () in
        let path,ch = Filename.open_temp_file ~temp_dir:cache_dir
            "entry" ".cache" in
        Data.Write.to_channel writer ch proj;
        Out_channel.close ch;
        {
          index with
          entries = Map.add index.entries ~key:id ~data:{
              size = size path; path; atime = ctime; ctime; hits = 1
            }
        }) in
  let load src =
    Index.with_entry src ~f:(fun e ->
        try
          report_progress ~note:"loading" ();
          let proj = In_channel.with_file e.path
              ~f:(Data.Read.of_channel reader) in
          let atime = Unix.time () in
          let hits = e.hits + 1 in
          report_progress ~note:"reindexing" ();
          Some {e with atime; hits}, Some proj
        with exn -> None,None) in
  Data.Cache.create ~load ~save


let main clean size show_info dir =
  Index.upgrade ();
  set_dir dir;
  if clean then cleanup ();
  if show_info then print_info ();
  info "caching to %s" (Index.cache_dir ());
  Option.iter size ~f:set_size;
  Data.Cache.Service.provide {Data.Cache.create}

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Provide caching service for all data types. The caching entry
         point is defined in the $(i,Data) module of the $(i,Regular)
         library.";
      `S "SEE ALSO";
      `P "$(b,regular)(3)";

    ] in
  let clean = Config.(flag "clean" ~doc:"Cleanup all caches") in
  let set_size = Config.(param (some int64) "size" ~docv:"N"
                           ~doc:"Set maximum total size of cached data to
                                 $(docv) MB. The option value will persist
                                 between different runs of the program") in
  let dir = Config.(param (some string) "dir" ~docv:"DIR"
                      ~doc:"Use $(docv) as a cache directory") in
  let print_info = Config.(flag "info" ~doc:"Print information about the
                                             cache and exit") in
  Config.when_ready (fun {Config.get=(!)} ->
      main !clean !set_size !print_info !dir)
