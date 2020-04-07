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
    mtime : float;
  }

  let t = ref None

  let mtime () =
    let s = Unix.stat @@ Index.index_file ()  in
    Unix.(s.st_mtime)

  let update index =
    let mtime = mtime () in
    t := Some {index; mtime }

  let write idx = Index.write idx

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
    write index;
    update index

  let reload () =
    match !t with
    | None -> read ()
    | Some t ->
      if t.mtime = mtime () then t.index
      else force_read ()

  let store ~f =
    let cache_dir = Index.cache_dir () in
    Index.with_lock ~f:(fun () ->
        let index = reload () in
        let index',data = f cache_dir index in
        write index';
        data)

  let load key ~f =
    let idx = read () in
    match Map.find idx.entries key with
    | None -> None
    | Some entry -> f entry

  let update ~f = store ~f:(fun dir idx -> f dir idx, ())

  let iter ~f = match !t with
    | None -> f (read ())
    | Some {index} -> f index
end

let size file = Unix.LargeFile.((stat file).st_size)

let cleanup () =
  Global.update ~f:(fun _ idx ->
      Map.iter idx.entries ~f:GC.remove_entry;
      {idx with entries = Data.Cache.Digest.Map.empty});
  exit 0

let overhead c =
  let open Int64 in
  of_float @@ (((to_float c.limit /. to_float c.max_size) -. 1.0) *. 100.)

let limit max_size overhead =
  Int64.(max_size + max_size * overhead / 100L)

let set_size size =
  Global.update ~f:(fun _ idx ->
      let overhead = overhead idx.config in
      let max_size = Int64.(size * 1024L * 1024L) in
      let limit = limit max_size overhead in
      {idx with config = {idx.config with max_size; limit}})

let set_overhead overhead =
  if Float.(overhead >= 0.0 && overhead < 1.0)
  then
    let overhead = Int64.of_float @@ overhead *. 100. in
    Global.update ~f:(fun _ idx ->
        let limit = limit idx.config.max_size overhead in
        {idx with config = {idx.config with limit}})
  else
    raise (Invalid_argument "Cache overhead should be in the range [0.0; 1.0) ")

let run_gc () =
  Global.update ~f:(fun _ idx -> GC.run idx)

let disable_gc x =
  Global.update ~f:(fun _ idx ->
      {idx with config = {idx.config with gc_enabled = not x}})

let print_info () =
  Global.iter ~f:(fun idx ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb idx.config.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb idx.current_size);
  exit 0

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Index.set_cache_dir dir

let create reader writer =
  let save id proj =
    Global.update ~f:(fun cache_dir idx ->
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
  Index.upgrade ();
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
