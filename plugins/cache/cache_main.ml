open Core_kernel
open Regular.Std
open Bap.Std
open Format
include Self()

module Filename = Caml.Filename

type entry = {
  atime   : float;
  ctime   : float;
  hits    : int;
  path    : string;
  size    : int64;
} [@@deriving bin_io, compare, sexp]

type entries = entry Data.Cache.Digest.Map.t

let t = Unix.gettimeofday
module Bench = My_bench

module Compatibility = struct
  module V2 = struct
    type config = {
      max_size : int64;
    } [@@deriving bin_io, compare, sexp]

    type t = {
      config  : config;
      entries : entry Data.Cache.Digest.Map.t;
    } [@@deriving bin_io, compare, sexp]
  end
end

type config = {
  max_size : int64;
  overhead : float;
  gc_enabled : bool;
} [@@deriving bin_io, compare, sexp]

type index = {
  current_size : int64;
  config  : config;
  entries : entry Data.Cache.Digest.Map.t;
} [@@deriving bin_io, compare, sexp]


let default_config = {
  max_size = 4_000_000_000L;
  overhead = 0.2;
  gc_enabled = true;
}

let empty = {
  config = default_config;
  entries = Data.Cache.Digest.Map.empty;
  current_size  = 0L;
}

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

module Entry = struct

  type t = entry
  [@@deriving bin_io, compare, sexp]

  let create path size =
    let ctime = Unix.time () in
    { size; path; atime = ctime; ctime; hits = 1 }

  let touch e =
    { e with atime = Unix.time (); hits = e.hits + 1 }

  let remove e =
    try Sys.remove e.path
    with exn ->
      warning "unable to remove entry: %s" (Exn.to_string exn)

end

module GC = struct

  (* freq is a frequence of accesses to the cache entry in
     access per second. *)
  let freq e =
    Float.(of_int e.hits / (e.atime - e.ctime))

  let remove_from_index idx key =
    match Map.find idx.entries key with
    | None -> idx
    | Some entry ->
      let current_size = Int64.(idx.current_size - entry.size) in
      { idx with entries = Map.remove idx.entries key; current_size }

  let remove_less_used_entry idx =
    Map.to_sequence idx.entries |>
    Seq.min_elt ~compare:(fun (_,e1) (_,e2) ->
        Float.compare (freq e1) (freq e2))
    |> function
    | None -> idx
    | Some (key,entry) ->
      Entry.remove entry;
      remove_from_index idx key

  let limit_of_config c =
    Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

  let time_to_clean idx =
    let limit = limit_of_config idx.config in
    Int64.(idx.current_size > limit)

  let clean idx =
    let rec loop idx =
      let size = idx.current_size in
      if Int64.(size > 0L) && Int64.(size > idx.config.max_size)
      then
        loop (remove_less_used_entry idx)
      else idx in
    loop idx

  let clean x = Bench.with_args1 clean x "clean"

  let clean idx =
    if time_to_clean idx
    then clean idx
    else idx

end

module IO = struct

  module T = struct
    type t = index [@@deriving bin_io]
  end

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

  let index_of_file file =
    let idx =
      try from_file (module T) file
      with e ->
        warning "read index: %s" (Exn.to_string e);
        empty in
    if idx.config.gc_enabled then
      GC.clean idx
    else idx

  let read = index_of_file

  let read'' =
    let index = ref empty in
    let mtime = ref 0.0 in
    let update s index' =
      mtime := Unix.(s.st_mtime);
      index := index' in
    fun file ->
      let stat = Unix.stat file in
      if Float.(!mtime = stat.st_mtime)
      then !index
      else
        let index' = index_of_file file in
        let () = update stat index' in
        index'

  let write file index =
    try to_file (module T) file index
    with e -> warning "store index: %s" (Exn.to_string e)

end

module Buffered_io = struct
  type t = {
    index  : index;
    file   : string;
    age    : int;
  }

  let max_age = 256
  let state = ref None

  let init file index =
    state := Some {index; file; age=0}

  let flush () = match !state with
    | None -> ()
    | Some s -> IO.write s.file s.index

  let write file index =
    match !state with
    | None -> init file index;
    | Some s ->
      if s.age mod max_age = 0 then
        flush ();
      state := Some { s with index; age = s.age + 1}

  let read file =
    match !state with
    | None ->
      let index = IO.read file in
      init file index;
      index
    | Some s -> s.index

  let () = at_exit flush

end

module BIO = Buffered_io


module Index = struct

  let index_version = 3
  let supported_versions = [3;2;1]
  let index_file = sprintf "index.%d" index_version
  let lock_file = "lock"

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

  let size (entries : entries) =
    Map.fold entries ~init:0L ~f:(fun ~key:_ ~data:e size ->
        Int64.(size + e.size))

  let save_to_index idx key entry =
    let current_size = Int64.(idx.current_size + entry.size) in
    { idx with entries = Map.set idx.entries key entry; current_size }

  let with_index ~f =
    let cache_dir = cache_dir () in
    let file = cache_dir / index_file in
    let lock = cache_dir / lock_file in
    let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
    Unix.lockf lock Unix.F_LOCK 0;
    protect ~f:(fun () ->
        let index = BIO.read file in
        let index',data = f cache_dir index in
        if not (phys_equal index index') then
          BIO.write file index';
        data)
      ~finally:(fun () ->
          Unix.(lockf lock F_ULOCK 0; close lock))

  let update ~f = with_index ~f:(fun dir idx -> f dir idx, ())
  let run ~f = with_index ~f:(fun _dir idx -> idx, f idx)

  let with_entry src ~f =
    with_index ~f:(fun _dir idx ->
        match Map.find idx.entries src with
        | None -> idx,None
        | Some entry ->
          let res = f entry in
          let entry = Entry.touch entry in
          { idx with
            entries = Map.update idx.entries src (fun _ -> entry)},
          res)

  let upgrade_index file version =
    let () = match version with
      | 1 ->
        warning "can't load index, version 1 is not supported anymore";
        BIO.write (cache_dir () / index_file) empty;
      | 2 ->
        let index = IO.from_file (module Compatibility.V2) file in
        let size = size index.entries in
        let index' = {
          config = default_config;
          current_size = size;
          entries = index.entries;
        } in
        BIO.write (cache_dir () / index_file) index'
      | x ->
        warning
          "can't update index version from %d to %d" x index_version in
    Sys.remove file

  let versions =
    List.map supported_versions ~f:(sprintf "index.%d")

  let find_index () =
    let dir = cache_dir () in
    let files = List.map versions ~f:(fun x -> dir / x) in
    List.find files ~f:Sys.file_exists

  let upgrade () =
    match find_index () with
    | None -> ()
    | Some file -> match get_version file with
      | Ok ver when Int.(ver = index_version) -> ()
      | Ok ver -> upgrade_index file ver
      | Error er ->
        error "unknown index version: %s" (Error.to_string_hum er)

  let size idx = size idx.entries

end

let size file =
  Unix.LargeFile.((stat file).st_size)

let cleanup () =
  Index.update ~f:(fun _ idx ->
      Map.iter idx.entries ~f:(fun e -> Entry.remove e);
      {idx with entries = Data.Cache.Digest.Map.empty});
  exit 0

let set_size size =
  Index.update ~f:(fun _ idx ->
      {idx with config = {idx.config with max_size = Int64.(size * 1024L * 1024L)}})

let set_overhead overhead =
  if Float.(overhead >= 0.0 && overhead < 1.0)
  then
    Index.update ~f:(fun _ idx ->
        {idx with config = {idx.config with overhead}})
  else
    raise (Invalid_argument "Cache overhead should be in the range [0.0; 1.0) ")

let run_gc () =
  Index.update ~f:(fun _ idx -> GC.clean idx)

let disable_gc flag =
  Index.update ~f:(fun _ idx ->
      {idx with config = {idx.config with gc_enabled = not flag}})

let print_info () =
  Index.run ~f:(fun idx ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb idx.config.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb (Index.size idx));
  exit 0

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> set_cache_dir dir

let create reader writer =
  let save id proj =
    Index.update ~f:(fun cache_dir index ->
        let ctime = Unix.time () in
        let path,ch = Filename.open_temp_file ~temp_dir:cache_dir
            "entry" ".cache" in
        Data.Write.to_channel writer ch proj;
        Out_channel.close ch;
        let entry = {
          size = size path; path; atime = ctime; ctime; hits = 1
        } in
        Index.save_to_index index id entry) in
  let load src =
    Index.with_entry src ~f:(fun e ->
        try
          report_progress ~note:"loading" ();
          let proj = In_channel.with_file e.path
              ~f:(Data.Read.of_channel reader) in
          report_progress ~note:"reindexing" ();
          Some proj
        with _exn -> None) in
  let save x y = Bench.with_args2 save x y "save" in
  let load x = Bench.with_args1 load x "load" in
  Data.Cache.create ~load ~save

let main clean show_info dir gc =
  Index.upgrade ();
  set_dir dir;
  if clean then cleanup ();
  if show_info then print_info ();
  info "caching to %s" (cache_dir ());
  if gc then run_gc ();
  Data.Cache.Service.provide {Data.Cache.create}

let update_config size overhead no_gc =
  Option.iter size ~f:set_size;
  Option.iter overhead ~f:set_overhead;
  Option.iter no_gc ~f:disable_gc;
  let is_set = Option.is_some in
  if is_set size || is_set overhead || is_set no_gc
  then
    let () = printf "Config updated, exiting ...\n" in
    exit 0

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
  let set_overhead =
    Config.(param (some float) "overhead"
              ~doc:"Set overhead for maximum total size of cached data,
              so the maximim total size = size + size * overhead") in
  let disable_gc = Config.(param (some bool) "disable-gc"
                             ~doc:"If set to true then disables GC") in
  let run_gc = Config.(flag "run-gc"
                         ~doc:"runs GC") in
  let dir = Config.(param (some string) "dir" ~docv:"DIR"
                      ~doc:"Use $(docv) as a cache directory") in
  let print_info = Config.(flag "info" ~doc:"Print information about the
                                             cache and exit") in
  Config.when_ready (fun {Config.get=(!)} ->
      update_config !set_size !set_overhead !disable_gc;
      main !clean !print_info !dir !run_gc)
