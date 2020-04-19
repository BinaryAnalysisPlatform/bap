open Core_kernel
open Regular.Std
open Bap.Std
open Format
open Bap_cache_types

include Self()

module Filename = Caml.Filename
module Cfg = Bap_cache_config
module Cache = Bap_cache
module GC = Cache.GC

let (/) = Filename.concat

let run_gc () =
  let cfg = Cache.read_config () in
  GC.shrink ~upto:cfg.max_size ()

let run_gc_with_threshold () =
  let cfg = Cache.read_config () in
  if cfg.gc_enabled then
    GC.shrink ~threshold:(Cfg.gc_threshold cfg)
      ~upto:cfg.max_size ()

let filename_of_digest = Data.Cache.Digest.to_string

let run_and_exit cmd = cmd (); exit 0

let print_info () =
  let cfg = Cache.read_config () in
  let size = Cache.size () in
  let mb s = Int64.(s / 1024L / 1024L) in
  printf "Maximum size: %5Ld MB@\n" @@ mb cfg.max_size;
  printf "Current size: %5Ld MB@\n" @@ mb size;
  printf "GC threshold: %5Ld MB@\n" @@ mb (Cfg.gc_threshold cfg);
  printf "Overhead:     %5g %%@\n" (cfg.overhead *. 100.0);
  printf "GC enabled:   %5b @\n" cfg.gc_enabled

let save writer dgst data =
  let dir = Cfg.cache_data () in
  let file = dir / filename_of_digest dgst in
  let tmp,ch = Filename.open_temp_file "entry" ".cache" in
  Data.Write.to_channel writer ch data;
  Out_channel.close ch;
  Sys.rename tmp file

let load reader dgst =
  let path = Cfg.cache_data () / filename_of_digest dgst in
  try
    Some (In_channel.with_file path
            ~f:(Data.Read.of_channel reader))
  with _ -> None

let create reader writer =
  Data.Cache.create
    ~load:(load reader)
    ~save:(save writer)

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Cfg.set_cache_dir dir

let main clean show_info gc =
  info "caching to %s" (Cfg.cache_dir ());
  if clean then run_and_exit GC.clean;
  if show_info then run_and_exit print_info;
  if gc then run_and_exit run_gc;
  Data.Cache.Service.provide {Data.Cache.create}

let size sz cfg = {cfg with max_size = Int64.(sz * 1024L * 1024L);}
let overhead ov cfg = {cfg with overhead = float ov /. 100.}
let disable_gc x cfg = {cfg with gc_enabled = not x}

let update_config sz ov gc =
  let set f x y = match x with
    | None -> y
    | Some x -> f x y in
  let cfg = Cache.read_config () in
  let cfg' = set size sz cfg |> set overhead ov |> set disable_gc gc in
  if cfg <> cfg'
  then
    let () = Cache.write_config cfg' in
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
    Config.(param (some int) "overhead"
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
      set_dir !dir;
      Cache.init ();
      run_gc_with_threshold ();
      update_config !set_size !set_overhead !disable_gc;
      main !clean !print_info !run_gc)
