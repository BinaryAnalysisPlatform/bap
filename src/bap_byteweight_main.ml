open Core_kernel.Std
open Bap.Std
open Frontend
open Bap_plugins.Std
open Result.Monad_infix
open Format

include Self()

(** list of posix regexes of files that are ignored during
    training.  *)
let ignored = [
  ".*\\.git.*";
  ".*/README.*";
  sprintf ".*\\.(%s)" @@ String.concat ~sep:"|" [
    "txt"; "html"; "htm"; "md"; "py"; "pyc"; "sh"
  ];
]

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures

let train_on_file meth length db path : (unit,'a) Result.t =
  Image.create path >>= fun (img,warns) ->
  let arch = Image.arch img in
  let symtab = Addr.Table.create () in
  Table.iteri (Image.symbols img) ~f:(fun mem _ ->
      Addr.Table.add_exn symtab ~key:(Memory.min_addr mem) ~data:());
  let test mem = Addr.Table.mem symtab (Memory.min_addr mem) in
  let bw = if not (Sys.file_exists db) then Ok (BW.create ())
    else match Sigs.load ~mode:"bytes" ~path:db arch with
      | Ok s -> if meth = `update
        then Ok (Binable.of_string (module BW) s)
        else Ok (BW.create ())
      | Error _ when meth = `rewrite -> Ok (BW.create ())
      | Error e ->
        Or_error.errorf "can't update entry, because %s"
          (Sigs.string_of_error e)  in
  bw >>= fun bw ->
  Table.iteri (Image.segments img) ~f:(fun mem sec ->
      if Image.Segment.is_executable sec then
        BW.train bw ~max_length:length test mem);
  let data = Binable.to_string (module BW) bw in
  Sigs.save ~mode:"bytes" ~path:db arch data |>
  Result.map_error ~f:(fun e ->
      Error.createf "signatures are not updated: %s" @@
      Sigs.string_of_error e)

let matching =
  let open FileUtil in
  let ignored =
    List.map ignored ~f:Re_posix.re |> Re.alt |> Re.compile in
  let matches s = Re.execp ignored s in
  let ignored = Custom matches in
  FileUtil.(And (Is_file, Not ignored))

let train meth length comp db paths =
  let db = Option.value db ~default:"sigs.zip" in
  let collect path =
    FileUtil.find matching path (fun xs x -> x :: xs) [] in
  let files = List.map paths
      ~f:(fun f -> if Sys.is_directory f then collect f else [f]) |>
              List.concat |> List.sort ~cmp:String.compare in
  let total = List.length files in
  let start = Sys.time () in
  let errors = ref 0 in
  List.iteri files ~f:(fun n path ->
      printf "[%3d / %3d] %-66s%!" (n+1) total path;
      let r = match train_on_file meth length db path with
        | Ok () -> "OK"
        | Error err ->
          eprintf "Error: %a\n%!" Error.pp err;
          (incr errors); "SKIPPED" in
      printf "%6s\n%!" r);
  printf "Processed %d files out of %d in %g seconds\n"
    (total - !errors) total (Sys.time () -. start);
  printf "Signatures are stored in %s\n%!" db;
  Ok ()

let create_bw img path =
  let arch = Image.arch img in
  Sigs.load ?path ~mode:"bytes" arch |>
  Result.map_error ~f:(fun e ->
      Error.createf "failed to read signatures from a database: %s"
        (Sigs.string_of_error e)) >>| fun data ->
  Binable.of_string (module BW) data

let find threshold length comp path input =
  Image.create input >>= fun (img,_warns) ->
  create_bw img path >>= fun bw ->
  Table.iteri (Image.segments img) ~f:(fun mem sec ->
      if Image.Segment.is_executable sec then
        let start = Memory.min_addr mem in
        let rec loop n =
          match BW.next bw ~length ~threshold mem n with
          | Some n -> printf "%a\n" Addr.pp Addr.(start ++ n); loop (n+1)
          | None -> () in
        loop 0);
  Ok ()

let symbols print_name print_size input =
  Image.create input >>| fun (img,_warns) ->
  let syms = Image.symbols img in
  Table.iteri syms ~f:(fun mem sym ->
      let addr = Memory.min_addr mem in
      let name = if print_name then Image.Symbol.name sym else "" in
      let size = if print_size
        then sprintf "%4d " (Memory.length mem) else "" in
      printf "%a %s%s\n" Addr.pp addr size name);
  printf "Outputted %d symbols\n" (Table.length syms)

let dump info length threshold path (input : string) =
  Image.create input >>= fun (img, _warns) ->
  match info with
  | `BW ->
    create_bw img path >>= fun bw ->
    let fs_set = Table.foldi (Image.segments img) ~init:Addr.Set.empty
        ~f:(fun mem sec fs_s ->
            if Image.Segment.is_executable sec then
              let new_fs_s = BW.find bw ~length ~threshold mem in
              Addr.Set.union fs_s @@ Addr.Set.of_list new_fs_s
            else fs_s) in
    Symbols.write_addrs stdout @@ Addr.Set.to_list fs_set;
    Ok ()
  | `SymTbl ->
    let syms = Image.symbols img in
    let mems = Table.regions syms in
    Symbols.write_addrs stdout @@ Seq.to_list (Seq.map mems Memory.min_addr);
    Ok ()

let create_parent_dir dst =
  let dir = if Filename.(check_suffix dst dir_sep)
    then dst else Filename.dirname dst in
  FileUtil.mkdir ~parent:true dir


let fetch fname url =
  let tmp,fd = Filename.open_temp_file "bap_" ".sigs" in
  let write s = Out_channel.output_string fd s; String.length s in
  let conn = Curl.init () in
  Curl.set_writefunction conn (write);
  Curl.set_failonerror conn true;
  Curl.set_followlocation conn true;
  Curl.set_sslverifypeer conn false;
  Curl.set_url conn url;
  Curl.perform conn;
  Curl.cleanup conn;
  Out_channel.close fd;
  create_parent_dir fname;
  FileUtil.mv tmp fname;
  printf "Successfully downloaded to %s\n" fname

let fetch fname url = try Ok (fetch fname url) with
  | Curl.CurlException (err,n,_) ->
    Or_error.errorf "failed to fetch: %s" (Curl.strerror err)
  | exn -> Or_error.of_exn exn

let install src dst = Or_error.try_with begin fun () ->
    create_parent_dir dst;
    FileUtil.cp [src] dst;
    printf "Installed signatures to %s\n" dst;
  end

let update url dst =
  let old = Filename.temp_file "bap_old_sigs" ".zip" in
  if Sys.file_exists dst
  then FileUtil.cp [dst] old;
  fetch dst url >>| fun () ->
  if FileUtil.cmp old dst = None
  then printf "Already up-to-date.\n"
  else printf "Installed new signatures.\n";
  if Sys.file_exists old then FileUtil.rm [old]

module Cmdline = struct

  let dumpc =
    let doc =
      "Dump the function starts in a given executable by given tool" in
    Config.(command "dump" ~doc)

  let trainc =
    let doc = "Train byteweight on the specified set of files" in
    Config.(command "train" ~doc)

  let findc =
    let doc = "Output all function starts in a given executable" in
    Config.(command "find" ~doc)

  let fetchc =
    let doc = "Fetch signatures from the specified url" in
    Config.(command "fetch" ~doc)

  let installc =
    let doc = "Install signatures" in
    Config.(command "install" ~doc)

  let updatec =
    let doc = "Download and install latest signatures" in
    Config.(command "update" ~doc)

  let symbolsc =
    let doc = "Print file's symbol table if any." in
    Config.(command "symbols" ~doc)

  let filename : string Config.param =
    let doc = "Input filename." in
    Config.(pos non_dir_file ~docv:"FILE" ~doc 0)

  let database_in : string option Config.param =
    let doc = "Path to signature database" in
    Config.(param (some non_dir_file) "d" ~synonyms:["db"] ~doc)

  let database : string option Config.param =
    let doc = "Update or create database at $(docv)" in
    Config.(param (some string) "db" ~synonyms:["d"]
              ~docv:"DBPATH" ~doc)

  let files : string list Config.param =
    let doc = "Input files and directories" in
    Config.(pos_all file ~docv:"FILE" ~doc ())

  let compiler : string option Config.param =
    let doc = "Assume the training set is compiled by $(docv)" in
    Config.(param (some string) "comp" ~docv:"COMPILER" ~doc)

  let length : int Config.param =
    let doc = "Maximum prefix length" in
    Config.(param int "length" ~synonyms:["l"] ~default:16 ~doc)

  let meth : [`update | `rewrite] Config.param =
    let enums = ["update", `update; "rewrite", `rewrite] in
    let doc = sprintf "If entry exists then %s it." @@
      Config.doc_enum enums in
    Config.(param (enum enums) "method" ~synonyms:["m"]
              ~default:`update ~doc)

  let threshold : float Config.param =
    let doc = "Decide that sequence starts a function \
               if m / n > $(docv),  where n is the total \
               number of occurences of a given sequence in \
               the training set, and m is how many times \
               it has occured at the function start position." in
    Config.(param float "threshold" ~synonyms:["t"]
              ~default:0.5 ~docv:"THRESHOLD" ~doc)

  let url : string Config.param =
    let doc = "Url of the binary signatures" in
    let default = sprintf
        "https://github.com/BinaryAnalysisPlatform/bap/\
         releases/download/v%s/sigs.zip" Config.version in
    Config.(param string "url" ~default ~doc)

  let src : string Config.param =
    Config.(pos non_dir_file ~default:"sigs.zip" ~docv:"SRC"
              ~doc:"Signatures file" 0)

  let dst : string Config.param =
    Config.(pos string ~default:Sigs.default_path ~docv:"DST"
              ~doc:"Destination" 1)

  let output : string Config.param =
    let doc = "Output filename" in
    Config.(param string "o" ~default:"sigs.zip" ~doc)

  let print_name : bool Config.param =
    let doc = "Print symbol's name." in
    Config.(flag "print-name" ~synonyms:["n"] ~doc)

  let print_size : bool Config.param =
    let doc = "Print symbol's size." in
    Config.(flag "print-size" ~synonyms:["s"] ~doc)

  let tool : [`BW | `SymTbl] Config.param =
    let enums = ["byteweight", `BW; "symbols", `SymTbl] in
    let doc = sprintf "The info to be dumped. %s"
      @@ Config.doc_enum enums in
    Config.(param (enum enums) "info" ~synonyms:["i"] ~default:`BW ~doc)

  let usage choices =
    eprintf "usage: bap-byteweight [--version] [--help] \
             <command> [<args>]\n";
    eprintf "where <command> := %s.\n"
      (String.concat ~sep:" | " choices)

  let () =
    let doc = "byteweight toolkit" in
    let man = [
      `S "DESCRIPTION";
      `P "A toolkit for training, fetching, testing
          and installing byteweight signatures.";
    ] in
    Config.descr doc;
    Config.(manpage default_command man);
    Config.(when_ready default_command (fun {Config.get=(!)} ->
        usage ["dump"; "train"; "find"; "fetch"; "install"; "update";
               "symbols"]));
    Config.when_ready dumpc (fun {Config.get=(!)} -> ok_exn (
        dump !tool !length !threshold !database_in !filename));
    Config.when_ready trainc (fun {Config.get=(!)} -> ok_exn (
        train !meth !length !compiler !database !files));
    Config.when_ready findc (fun {Config.get=(!)} -> ok_exn (
        find !threshold !length !compiler !database_in !filename));
    Config.when_ready fetchc (fun {Config.get=(!)} -> ok_exn (
        fetch !output !url));
    Config.when_ready installc (fun {Config.get=(!)} -> ok_exn (
        install !src !dst));
    Config.when_ready updatec (fun {Config.get=(!)} -> ok_exn (
        update !url !dst));
    Config.when_ready symbolsc (fun {Config.get=(!)} -> ok_exn (
        symbols !print_name !print_size !filename));
    Plugins.run ()

end
