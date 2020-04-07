open Core_kernel
open Regular.Std
open Bap.Std
open Format
include Self()

module Filename = Caml.Filename

open Cache_types

module Index = Cache_index
module GC = Cache_gc

let (/) = Filename.concat

module Global = struct

  type t = {
    index : index;
    mtime : float option;
  }

  let t = ref None

  let mtime () =
    let index = Index.index_file () in
    if Sys.file_exists index then
      let s = Unix.stat index in
      Some Unix.(s.st_mtime)
    else None

  let update index =
    let mtime = mtime () in
    t := Some {index; mtime }

  let read () = match !t with
    | None ->
      let idx = Index.read () in
      let idx =
        if idx.config.gc_enabled then
          GC.run idx
        else idx in
      update idx;
      idx
    | Some {index} -> index

  let force_read () =
    t := None;
    read ()

  let write index =
    Index.write index;
    update index

  let reload () =
    match !t with
    | None -> read ()
    | Some t ->
      if t.mtime = mtime () then t.index
      else force_read ()

  let store ~f =
    Index.with_lock ~f:(fun () ->
        let index = reload () in
        let index',data = f index in
        write index';
        data)

  let load key ~f =
    let idx = read () in
    match Map.find idx.entries key with
    | None -> None
    | Some entry -> f entry

  let update ~f = store ~f:(fun idx -> f idx, ())

  let iter ~f = match !t with
    | None -> f (read ())
    | Some {index} -> f index
end

let size file = Unix.LargeFile.((stat file).st_size)

let cleanup () =
  Global.update ~f:(fun idx ->
      Map.iter idx.entries ~f:GC.remove_entry;
      {idx with entries = Data.Cache.Digest.Map.empty});
  exit 0

let set_size size =
  Global.update ~f:(fun idx ->
      let max_size = Int64.(size * 1024L * 1024L) in
      {idx with config = {idx.config with max_size;}})

let set_overhead overhead =
  if Float.(overhead >= 0.0 && overhead < 1.0)
  then
    Global.update ~f:(fun idx ->
        {idx with config = {idx.config with overhead}})
  else
    raise (Invalid_argument "Cache overhead should be in the range [0.0; 1.0) ")

let run_gc () =
  Global.update ~f:(fun idx -> GC.run idx);
  exit 0

let disable_gc x =
  Global.update ~f:(fun idx ->
      {idx with config = {idx.config with gc_enabled = not x}})


let limit_of_config c =
  Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

let print_info () =
  Global.iter ~f:(fun idx ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb idx.config.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb idx.current_size;
      printf "GC threshold: %5Ld MB@\n" @@ mb (GC.threshold idx.config);
      printf "Overhead:     %g %%@\n" (idx.config.overhead *. 100.0);
      printf "GC enabled:   %b@\n" idx.config.gc_enabled);
  exit 0

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Index.set_cache_dir dir

let create reader writer =
  let save id proj =
    Global.update ~f:(fun idx ->
        let cache_dir = Index.cache_dir () in
        let path,ch = Filename.open_temp_file ~temp_dir:cache_dir
            "entry" ".cache" in
        Data.Write.to_channel writer ch proj;
        Out_channel.close ch;
        let entry = { size = size path; path; } in
        let current_size = Int64.(idx.current_size + entry.size) in
        { idx with entries =
                     Map.set idx.entries id entry; current_size }) in
  let load src =
    Global.load src ~f:(fun e ->
        try
          report_progress ~note:"loading" ();
          let proj = In_channel.with_file e.path
              ~f:(Data.Read.of_channel reader) in
          report_progress ~note:"reindexing" ();
          Some proj
        with _exn -> None) in
  Data.Cache.create ~load ~save

let main clean show_info dir gc =
  set_dir dir;
  if clean then cleanup ();
  if show_info then print_info ();
  info "caching to %s" (Index.cache_dir ());
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
              ~doc:"Set overhead of maximum total size of cached data,
              so the size after GC will start to clean cache directory
              is size + size * overhead.The option value will persist
                                  between different runs of the program") in
  let disable_gc = Config.(param (some bool) "disable-gc"
                             ~as_flag:(Some true)
                             ~doc:"Disables the garbage collector
                                  The option value will persist
                                  between different runs of the program") in
  let run_gc = Config.(flag "run-gc" ~doc:"runs garbage collector") in
  let dir = Config.(param (some string) "dir" ~docv:"DIR"
                      ~doc:"Use $(docv) as a cache directory") in
  let print_info = Config.(flag "info" ~doc:"Print information about the
                                             cache and exit") in
  Config.when_ready (fun {Config.get=(!)} ->
      Index.upgrade ();
      update_config !set_size !set_overhead !disable_gc;
      main !clean !print_info !dir !run_gc)
