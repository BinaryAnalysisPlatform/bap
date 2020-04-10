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

  type t = {
    mtime : float;
    size  : int64;
    cfg   : config;
  }

  let t = ref None

  let threshold c =
    Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

  let run_gc t =
    if t.cfg.gc_enabled && Int64.(t.size > threshold t.cfg) then
      let () = GC.shrink ~upto:t.cfg.max_size in
      {t with size = Cache.size (); }
    else t

  let read () = match !t with
    | None ->
      let cfg = Cfg.read () in
      let mtime = Cache.mtime () in
      let size  = Cache.size () in
      let t' = {mtime; size; cfg} in
      t := Some t';
      t'
    | Some t -> t

  let force_read () =
    t := None;
    read ()

  let size file = Unix.LargeFile.( (stat file).st_size )

  let store writer data =
    let tmp,ch = Filename.open_temp_file "entry" ".cache" in
    Data.Write.to_channel writer ch data;
    Out_channel.close ch;
    tmp

  let filename_of_digest = Data.Cache.Digest.to_string

  let store_entry digest writer data =
    let t' = match !t with
      | None -> read ()
      | Some t ->
        let tm = Cache.mtime () in
        let t =
          if Float.(t.mtime < tm)
          then force_read ()
          else t in
        run_gc t in
    let tmp = store writer data in
    let t' = {t' with size = Int64.(t'.size + size tmp) } in
    Sys.rename tmp (Cfg.cache_dir () / filename_of_digest digest);
    t := Some {t' with mtime = Cache.mtime ()}

  let load_entry reader digest =
    let path = Cfg.cache_dir () / filename_of_digest digest in
    try
      Some (In_channel.with_file path ~f:(Data.Read.of_channel reader))
    with _ -> None

  let iter ~f = match !t with
    | None -> f (read ())
    | Some t -> f t

end

let cleanup () = GC.clean (); exit 0

let update_config ~f = Cfg.write @@ f (Cfg.read ())

let set_size size =
  update_config ~f:(fun cfg ->
      let max_size = Int64.(size * 1024L * 1024L) in
      {cfg with max_size;})

let set_overhead overhead =
  update_config ~f:(fun cfg -> {cfg with overhead})

let run_gc () =
  let cfg = Cfg.read () in
  GC.shrink ~upto:cfg.max_size;
  exit 0

let disable_gc x =
  update_config ~f:(fun cfg -> {cfg with gc_enabled = not x})

let print_info () =
  Global.iter ~f:(fun {cfg;size} ->
      let mb s = Int64.(s / 1024L / 1024L) in
      printf "Maximum size: %5Ld MB@\n" @@ mb cfg.max_size;
      printf "Current size: %5Ld MB@\n" @@ mb size;
      printf "GC threshold: %5Ld MB@\n" @@ mb (Global.threshold cfg);
      printf "Overhead:     %5g %%@\n" (cfg.overhead *. 100.0);
      printf "GC enabled:   %5b @\n" cfg.gc_enabled);
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
              ~doc:"Controls the aggressiveness of the garbage collector.
                    The higher the number the more space will be
                    wasted but the cache system will run faster. It is
                    expressed as a percentage of the max-size parameter") in
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
