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
  printf "Overhead:     %5g %%@\n" (cfg.overhead *. 100.0);
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

let size sz cfg = {cfg with capacity = sz}
let overhead ov cfg = {cfg with overhead = float ov /. 100.}
let disable_gc x cfg = {cfg with gc_enabled = not x}
let enable_gc x cfg = {cfg with gc_enabled = x}

let update_config sz ov no_gc gc =
  let set f x y = match x with
    | None -> y
    | Some x -> f x y in
  let is_set = Option.is_some in
  let cfg = Cache.read_config () in
  let cfg' =
    set size sz cfg |>
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


let doc = "
# DESCRIPTION

Provide caching service for all data types. The caching entry
point is defined in the [Data] module of the [Regular] library.

The cache plugin implements lock-free store/loading operations with
O(1) complexity: the same cache folder can be safely shared
between different processes without any performance impact and
these operations don't depend from the cache size.

Also, the plugin maintain the cache size below the certain level,
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


let () =
  let open Syntax in
  documentation doc;
  let dir = Configuration.parameter
      ~doc:"Use provided folder as a cache directory"
      Extension.Type.("DIR" %: some string) "dir" in
  let clean = Configuration.flag
      ~doc:
        "Cleanup all caches.
         Deprecated, use $(b, bap cache --clean) instead" "clean" in
  Bap_main.Extension.declare @@ fun ctxt ->
  init (ctxt --> dir);
  if ctxt --> clean then
    run_and_exit GC.clean;
  provide_service ();
  Ok ()

let opts = ref String.Map.empty

let update name ?arg short_descr doc =
  let name = match arg with
    | None -> name
    | Some arg -> sprintf "%s=%s" name arg in
  let descr = Option.value ~default:doc short_descr in
  opts := Map.add_exn !opts name descr

let parameter ?arg ?aliases ?as_flag ?short_descr ~doc typ name =
  update name ?arg short_descr doc;
  Command.parameter ?aliases ?as_flag ~doc typ name

let flag ?short_descr ~doc name =
  update name short_descr doc;
  Command.flag ~doc name

let dir =
  let arg = "DIR" in
  parameter ~arg ~doc:"use <DIR> as a cache directory"
    Extension.Type.(arg %: some string) "dir"

let clean  = flag ~doc:"cleanup the cache" "clean"
let run_gc = flag ~doc:"runs garbage collector and exits" "run-gc"
let info = flag ~doc:"prints information about the cache and exits" "info"

let enable_gc =
  parameter ~as_flag:(Some true)
    ~short_descr:"enables garbage collector"
    ~doc:"enables garbage collector. The option value will persist
          between different runs of the program"
    Extension.Type.(some bool) "enable-gc"

let disable_gc =
  parameter ~as_flag:(Some true)
    ~short_descr:"disables garbage collector"
    ~doc:"disables garbage collector. The option value will persist
          between different runs of the program"
    Extension.Type.(some bool) "disable-gc"

let size =
  parameter
    ~arg:"N"
    ~short_descr:"sets the capacity of the cached data to <N> Mb"
    ~doc:"Set the capacity of cached data in Mb.
          The option value will persist
          between different runs of the program"
    Extension.Type.("N" %: some int) ~aliases:["size"] "capacity"

let overhead =
  parameter
    ~arg:"P"
    ~short_descr:"sets capacity overhead to <P> percents"
    ~doc:"Controls the aggressiveness of the garbage collector.
     The higher the number the more space will be
     wasted but the cache system will run faster. It is
     expressed as a percentage of the capacity parameter.
     The option value will persist between different runs of
     the program"
    Extension.Type.("P" %: some int) "overhead"

let print_command_options () =
  Format.printf "Command options:\n";
  Map.iteri !opts ~f:(fun ~key:name ~data:descr ->
      Format.printf  "  --%-24s %s@\n" name descr)

let _cmd =
  let doc =
    "provides options to control cache size and cache garbage collector." in
  Extension.Command.(
    begin
      declare ~doc "cache"
        (args $dir $clean $size $overhead $run_gc $disable_gc
         $enable_gc $info)
        (fun dir clean size overhead run_gc disable_gc enable_gc info
          _ctxt ->
          init dir;
          update_config size overhead disable_gc enable_gc;
          run clean info run_gc;
          print_command_options ();
          Ok ())

    end)
