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
  GC.shrink ~upto:cfg.max_size ()

let run_gc_with_threshold () =
  let cfg = Cache.read_config () in
  if cfg.gc_enabled then
    GC.shrink ~threshold:(Cache.gc_threshold cfg)
      ~upto:cfg.max_size ()

let filename_of_digest = Data.Cache.Digest.to_string

let run_and_exit cmd = cmd (); exit 0

let print_info () =
  let cfg  = Cache.read_config () in
  let () = printf "reading size %d\n" cfg.max_size in
  let size = Cache.size () in
  printf "Maximum size: %5d MB@\n" cfg.max_size;
  printf "Current size: %5d MB@\n" size;
  printf "GC threshold: %5d MB@\n" (Cache.gc_threshold cfg);
  printf "Overhead:     %5g %%@\n" (cfg.overhead *. 100.0);
  printf "GC enabled:   %5b @\n" cfg.gc_enabled

let save writer dgst data =
  let dir = Cache.data () in
  let file = dir / filename_of_digest dgst in
  Utils.to_file' ~temp_dir:dir writer file data

let load reader dgst =
  let path = Cache.data () / filename_of_digest dgst in
  try Some (Utils.from_file' reader path)
  with _exn -> None

let create reader writer =
  Data.Cache.create
    ~load:(load reader)
    ~save:(save writer)

let set_dir dir = match dir with
  | None -> ()
  | Some dir -> Cache.set_root dir

let clean_cache () =
  printf "cleaning cache ... \n%!";
  run_and_exit GC.clean

let main clean show_info gc =
  info "caching to %s" (Cache.root ());
  if clean then run_and_exit GC.clean;
  if show_info then run_and_exit print_info;
  if gc then run_and_exit run_gc;
  Data.Cache.Service.provide {Data.Cache.create}

let size sz cfg = {cfg with max_size = sz}
let overhead ov cfg = {cfg with overhead = float ov /. 100.}

let disable_gc x cfg =
  printf "disable gc\n";
  {cfg with gc_enabled = not x}

let enable_gc x cfg =
  printf "enable gc\n";
  {cfg with gc_enabled = x}

let update_config sz ov no_gc gc =
  let set f x y = match x with
    | None -> y
    | Some x -> f x y in
  let cfg = Cache.read_config () in
  let cfg' =
    set size sz cfg |>
    set overhead ov |>
    set disable_gc no_gc |>
    set enable_gc gc in
  if cfg <> cfg'
  then
    begin
      let () = printf "writing cfg with size %d\n" cfg'.max_size in
      Cache.write_config cfg';
      exit 0
    end

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
  let clean_old_style = Config.(flag "clean" ~doc:"Cleanup all caches") in
  Config.when_ready (fun {Config.get=(!)} ->
      if !clean_old_style then
        run_and_exit GC.clean)

open Extension


let dir =
  Command.parameter
    ~doc:"Use <DIR> as a cache directory"
    Extension.Type.("DIR" %: some string) "dir"

let clean = Command.flag ~doc:"Cleanup all caches" "clean"

let run_gc = Command.flag ~doc:"runs garbage collector" "run-gc"

let enable_gc =
  Command.parameter ~as_flag:(Some true)
    ~doc:"enables garbage collector"
    Extension.Type.("GC" %: some bool)
    "enable-gc"

let disable_gc =
  Command.parameter ~as_flag:(Some true)
    ~doc:"disables garbage collector"
    Extension.Type.("GC" %: some bool)
    "disable-gc"

let size =
  Command.parameter
    ~doc:"Set maximum total size of cached data in Mb.
          The option value will persist
          between different runs of the program"
    Extension.Type.("N" %: some int) "size"

let info =
  Command.flag "print-info"
    ~doc:"Print information about the cache and exit"

let overhead =
  Command.parameter
    ~doc:"Controls the aggressiveness of the garbage collector.
     The higher the number the more space will be
     wasted but the cache system will run faster. It is
     expressed as a percentage of the max-size parameter"
    Extension.Type.("N" %: some int) "overhead"


let man = "
# DESCRIPTION

Provide caching service for all data types. The caching entry
point is defined in the [Data] module of the [Regular] library.

The cache plugin implements lock-free, O(1) store/loading operations.
O(1) complexity means that store/load operations don't
depend from the cache size. And lock-free means that the same cache
folder can be safely shared between different processes without
any performance impact.

Also, the plugin maintain the cache size on a certain level
(unless the garbage collector is manually disabled via
$(b,--disable-gc) option.
The maximum occupied size of the cache is controlled by
the $(b,size) and $(b,overhead) parameters and can be expressed
with the next formula: $(b, total-size = size + overhead * size),
where $(b,size) is a max cache size in Mb and $(b,overhead)
is allowed percentage of excess.

Finally, GC is automatically launched once per every bap process and
randomly removes $(b, 2 * size * overhead) of cache entries with
prioritizing larger onces.
"


let _cmd =
  Extension.Command.(
    begin
      declare ~doc:man "cache"
        (args $dir $clean $size $overhead $run_gc $disable_gc
         $enable_gc $info)
        (fun dir clean size overhead run_gc disable_gc enable_gc info _ctxt ->
           set_dir dir;
           Cache.init ();
           run_gc_with_threshold ();
           update_config size overhead disable_gc enable_gc;
           main clean info run_gc;
           Ok ())

    end)
