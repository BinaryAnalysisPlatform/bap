open Core_kernel
open Regular.Std
open Bap.Std
open Format
open Bap_cache_types
open Bap_main

include Self()

module Filename = Caml.Filename
module Cache = Bap_cache
module Utils = Bap_cache_utils
module GC = Bap_cache_gc

let (/) = Filename.concat

let run_gc () =
  let cfg = Cache.read_config () in
  GC.shrink cfg

let run_gc_with_threshold () =
  let cfg = Cache.read_config () in
  if cfg.gc_enabled then
    GC.shrink ~by_threshold:true cfg

let filename_of_digest = Data.Cache.Digest.to_string
let run_and_exit cmd = cmd (); exit 0

let print_info () =
  let cfg  = Cache.read_config () in
  let size = Cache.size () in
  printf "Capacity:     %5d MB@\n" cfg.capacity;
  printf "Current size: %5d MB@\n" size;
  printf "GC threshold: %5d MB@\n" (Cache.gc_threshold cfg);
  printf "Overhead:     %5d %%@\n" cfg.overhead;
  printf "GC enabled:   %5b @\n" cfg.gc_enabled

let save writer dgst data =
  let dir = Cache.data () in
  let file = dir / filename_of_digest dgst in
  Utils.write_to_file ~temp_dir:dir writer file data

let load reader dgst =
  let path = Cache.data () / filename_of_digest dgst in
  try Some (Utils.read_from_file reader path)
  with _exn -> None

let create reader writer =
  Data.Cache.create
    ~load:(load reader)
    ~save:(save writer)

let provide_service () =
  info "caching to %s" (Cache.root ());
  Data.Cache.Service.provide {Data.Cache.create}

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Cache.set_root dir

let run clean show_info gc =
  if clean then run_and_exit GC.clean;
  if show_info then run_and_exit print_info;
  if gc then run_and_exit run_gc

let capacity sz cfg = {cfg with capacity = sz}
let overhead ov cfg = {cfg with overhead = ov}
let disable_gc x cfg = {cfg with gc_enabled = not x}
let enable_gc x cfg = {cfg with gc_enabled = x}

let update_config sz ov no_gc gc =
  let set f x y = match x with
    | None -> y
    | Some x -> f x y in
  let is_set = Option.is_some in
  let cfg = Cache.read_config () in
  let cfg' =
    set capacity sz cfg |>
    set overhead ov |>
    set disable_gc no_gc |>
    set enable_gc gc in
  if is_set sz || is_set ov || is_set no_gc || is_set gc
  then
    begin
      Cache.write_config cfg';
      exit 0
    end

let init dir =
  set_dir dir;
  Cache.init ();
  run_gc_with_threshold ()

open Extension
open Syntax

let doc = "
# DESCRIPTION

Provide caching service for all data types. The caching entry
point is defined in the [Data] module of the [Regular] library.

The cache plugin implements lock-free store/loading operations with
O(1) complexity: the same cache folder can be safely shared
between different processes without any performance impact and
these operations don't depend on the cache size.

Also, the plugin maintains the cache size below the certain level,
unless the garbage collector is manually disabled via
$(b,--disable-gc) option.
The maximum occupied size of the cache is controlled by
the $(b,size) and $(b,overhead) parameters and can be expressed
as $(b, total-size = size + overhead * size),
where $(b,size) is a cache capacity in Mb and $(b,overhead)
is the percentage of allowed excess. Once the cache size reaches
this maximum, $(b, 2 * capacity * overhead) of entries will be
removed by GC on the next program run.

GC is also lock-free and removes cache entries randomly with
prioritizing larger ones.

# SEE ALSO

$(b,regular)(3)
"

let cache_dir = Configuration.parameter
    ~doc:"Use provided folder as a cache directory"
    Extension.Type.("DIR" %: some string) "dir"

let () =
  documentation doc;
  let clean = Configuration.flag
      ~doc:
        "Cleanup all caches.
         Deprecated, use $(b, bap cache --clean) instead" "clean" in
  Bap_main.Extension.declare @@ fun ctxt ->
  init (ctxt --> cache_dir);
  if ctxt --> clean then
    run_and_exit GC.clean;
  provide_service ();
  Ok ()

let opts = ref String.Map.empty

let add_description ?arg name descr  =
  let name = match arg with
    | None -> name
    | Some arg -> sprintf "%s=%s" name arg in
  let name = "--" ^ name in
  opts := Map.add_exn !opts name descr

let flag doc name =
  add_description name doc;
  Command.flag ~doc name

let clean = flag "cleanup the cache" "clean"
let run_gc = flag "runs garbage collector and exits" "run-gc"
let info = flag "prints information about the cache and exits" "info"

let enable_gc =
  let name = "enable-gc" in
  add_description name "enables garbage collector";
  Command.parameter ~as_flag:(Some true)
    ~doc:"enables garbage collector. The option value will persist
          between different runs of the program"
    Extension.Type.(some bool) name

let disable_gc =
  let name = "disable-gc" in
  add_description name "disables garbage collector";
  Command.parameter ~as_flag:(Some true)
    ~doc:"disables garbage collector. The option value will persist
          between different runs of the program"
    Extension.Type.(some bool) name

let capacity =
  let name = "capacity" in
  add_description ~arg:"N" name
    "sets the capacity of the cached data to <N> Mb";
  Command.parameter
    ~doc:"Set the capacity of cached data in Mb.
          The option value will persist
          between different runs of the program"
    Extension.Type.("N" %: some int) ~aliases:["size"] name

let overhead =
  let name = "overhead" in
  add_description ~arg:"P" name
    "sets capacity overhead to <P> percents";
  Command.parameter
    ~doc:"Controls the aggressiveness of the garbage collector.
     The higher the number the more space will be
     wasted but the cache system will run faster. It is
     expressed as a percentage of the capacity parameter.
     The option value will persist between different runs of
     the program"
    Extension.Type.("N" %: some int) name

let print_command_options () =
  Format.printf "Command options:\n";
  Map.iteri !opts ~f:(fun ~key:name ~data:descr ->
      Format.printf  "  %-24s %s@\n" name descr)

let _cmd =
  let doc =
    "provides options to control cache size and cache garbage collector." in
  Extension.Command.(
    begin
      declare ~doc "cache"
        (args $clean $capacity $overhead $run_gc $disable_gc
         $enable_gc $info)
        (fun clean capacity overhead run_gc disable_gc enable_gc info
          ctxt ->
          init (ctxt --> cache_dir);
          update_config capacity overhead disable_gc enable_gc;
          run clean info run_gc;
          print_command_options ();
          Ok ())
    end)
