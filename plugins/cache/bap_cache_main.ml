open Core_kernel
open Regular.Std
open Bap.Std
open Format
include Self()

module Filename = Caml.Filename

open Bap_cache_types

module Cfg = Bap_cache_config
module Cache = Bap_cache
module GC = Cache.GC

let (/) = Filename.concat

module Global = struct

  open Cache

  type t = {
    mtime : float;
    size  : int64;
    cfg   : config;
  }

  let t = ref None

  let threshold c =
    Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

  let run_gc t =
    if t.cfg.gc_enabled then
      if Int64.(t.size  > threshold t.cfg) then
        let () = GC.remove Int64.(t.size - t.cfg.max_size) in
        {t with size = Cache.size ()}
      else t
    else t

  let read () = match !t with
    | None ->
      let cfg = Cfg.read () in
      let mtime = mtime () in
      let size  = size () in
      let t' = {mtime; size; cfg} in
      t := Some t';
      t'
    | Some t -> t

  let force_read () =
    t := None;
    read ()

  let store_entry filename writer data =
    let cache_dir = Cfg.cache_dir () in
    let tmp,ch = Filename.open_temp_file ~temp_dir:cache_dir
        "entry" ".cache" in
    Data.Write.to_channel writer ch data;
    Out_channel.close ch;
    Sys.rename tmp (cache_dir / filename)

  let update t' entry =
    let s = Unix.stat @@ Cfg.cache_dir () / entry in
    let mtime = Unix.(s.st_mtime) in
    let size = Unix.(s.st_size) in
    t := Some {t' with size = Int64.(t'.size + of_int size); mtime}

  let store_entry digest writer data =
    let filename = Data.Cache.Digest.to_string digest in
    let tm = mtime () in
    match !t with
    | None ->
      let t = read () in
      store_entry filename writer data;
      update t filename
    | Some t ->
      let t' =
        if Float.(t.mtime < tm) then
          force_read ()
        else t in
      let t' = run_gc t' in
      store_entry filename writer data;
      update t' filename

  let load_entry reader filename =
    let path = Cfg.cache_dir () / Data.Cache.Digest.to_string filename in
    if Sys.file_exists path then
      Some (In_channel.with_file path
              ~f:(Data.Read.of_channel reader))
    else None

  let update_config ~f =
    let t' = read () in
    let cfg = f t'.cfg in
    Cfg.write cfg;
    t := Some {t' with cfg }

  let iter ~f = match !t with
    | None -> f (read ())
    | Some t -> f t

end

let size file = Unix.LargeFile.((stat file).st_size)

let cleanup () =
  GC.remove_all ();
  exit 0

let set_size size =
  Global.update_config ~f:(fun cfg ->
      let max_size = Int64.(size * 1024L * 1024L) in
      {cfg with max_size;})

let set_overhead overhead =
  Global.update_config ~f:(fun cfg -> {cfg with overhead})

let run_gc () =
  let t = Global.read () in
  if Int64.(t.size > t.cfg.max_size) then
    GC.remove Int64.(t.size - t.cfg.max_size);
  exit 0

let disable_gc x =
  Global.update_config ~f:(fun cfg -> {cfg with gc_enabled = not x})

let print_info () =
  Global.iter ~f:(fun {cfg;size} ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb cfg.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb size;
      printf "GC threshold: %5Ld MB@\n" @@ mb (Global.threshold cfg);
      printf "Overhead:     %g %%@\n" (cfg.overhead *. 100.0);
      printf "GC enabled:   %b@\n" cfg.gc_enabled);
  exit 0

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Cfg.set_cache_dir dir

let create reader writer =
  let save id proj = Global.store_entry id writer proj in
  let load src = Global.load_entry reader src in
  Data.Cache.create ~load ~save

let main clean show_info dir gc =
  set_dir dir;
  if clean then cleanup ();
  if show_info then print_info ();
  info "caching to %s" (Cfg.cache_dir ());
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
      Cache.upgrade ();
      update_config !set_size !set_overhead !disable_gc;
      main !clean !print_info !dir !run_gc)
