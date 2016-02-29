open Core_kernel.Std
open Regular.Std
open Bap.Std
open Format
open Cmdliner
include Self()

type entry = {
  atime   : float;
  ctime   : float;
  hits    : int;
  path    : string;
  size    : int;
} with bin_io, compare, sexp

type config = {
  max_size : int;
} with bin_io, compare, sexp

type index = {
  config  : config;
  entries : entry String.Map.t;
} with bin_io, compare, sexp

let (/) = Filename.concat

module Index = struct
  let index_file = "index"
  let lock_file = "lock"
  let default_config = {
    max_size = 5_000_000_000;
  }
  let empty = {
    config = default_config;
    entries = String.Map.empty;
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
    Map.fold idx.entries ~init:0 ~f:(fun ~key:_ ~data:e size ->
        size + e.size)

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
    if size > 0 && size > idx.config.max_size
    then clean (evict_entry idx)
    else idx

  let remove_entry e =
    Sys.remove e.path

  let remove_files old_index new_index =
    Map.iter old_index.entries ~f:(fun ~key ~data:e ->
        if not (Map.mem new_index.entries key)
        then remove_entry e)

  let with_index ~f =
    let cache_dir = cache_dir () in
    let file = cache_dir / index_file in
    let lock = cache_dir / lock_file in
    let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o640 in
    Unix.lockf lock Unix.F_LOCK 0;
    protect ~f:(fun () ->
        let init = try Sexp.load_sexp file |> index_of_sexp with
          | _ -> empty in
        let index',data = f cache_dir init in
        remove_files init index';
        let index = clean index' in
        remove_files index' index;
        Sexp.save_hum file (sexp_of_index index);
        data)
      ~finally:(fun () -> Unix.lockf lock Unix.F_ULOCK 0)


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

end

let size file =
  Unix.((stat file).st_size)

let cleanup () =
  Index.update ~f:(fun _ idx ->
      {idx with entries = String.Map.empty});
  exit 0

let set_size size =
  Index.update ~f:(fun _ idx ->
      {idx with config = {max_size = size * 1024 * 1024}});
  exit 0

let print_info () =
  Index.run ~f:(fun idx ->
      let mb s = Int.(s / 1024 / 1024) in
      printf "Maximum size: %5d MB@\n" @@ mb idx.config.max_size;
      printf "Current size: %5d MB@\n" @@ mb (Index.size idx));
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
          let proj = In_channel.with_file e.path
              ~f:(Data.Read.of_channel reader) in
          let atime = Unix.time () in
          let hits = e.hits + 1 in
          Some {e with atime; hits}, Some proj
        with exn -> None,None) in
  Data.Cache.create ~load ~save


let main clean size info dir =
  set_dir dir;
  if clean then cleanup ();
  if info then print_info ();
  Option.iter size ~f:set_size;
  Data.Cache.Service.provide {Data.Cache.create}

let man = [
  `S "DESCRIPTION";
  `P "Provide caching service for all data types."
]

let info = Term.info name ~version ~doc ~man

let clean =
  let doc = "Cleanup all caches" in
  Arg.(value & flag & info ["clean"] ~doc)

let set_size =
  let doc =
    "Set maximum total size of cached data to $(docv) MB. The
  option value will persist between different runs of the program" in
  Arg.(value & opt (some int) None & info ["size"] ~doc ~docv:"N")

let dir =
  let doc = "Use $(docv) as a cache directory" in
  Arg.(value & opt (some string) None & info ["dir"] ~doc ~docv:"DIR")

let print_info =
  let doc = "Print information about the cache and exit" in
  Arg.(value & flag & info ["info"] ~doc)

let () =
  let spec = Term.(const main $clean $set_size $print_info $dir) in
  match Term.eval ~argv ~catch:false (spec,info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
