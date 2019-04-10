open Core_kernel
open Bap.Std
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

let load ?comp meth db arch =
  if not (Sys.file_exists db) then Ok (BW.create ())
  else match Sigs.load ?comp ~mode:"bytes" ~path:db arch with
       | Ok s -> if meth = `update
          then Ok (Binable.of_string (module BW) (Bytes.to_string s))
          else Ok (BW.create ())
    | Error (`No_entry _) -> Ok (BW.create ())
    | Error e ->
      Or_error.errorf "can't update entry, because %s"
        (Sigs.string_of_error e)

let train_on_file comp meth length db path : (unit,'a) Result.t =
  Image.create path >>= fun (img,_warns) ->
  let arch = Image.arch img in
  let symtab = Addr.Table.create () in
  Table.iteri (Image.symbols img) ~f:(fun mem _ ->
      Addr.Table.add_exn symtab ~key:(Memory.min_addr mem) ~data:());
  let test mem = Addr.Table.mem symtab (Memory.min_addr mem) in
  let bws =
    let default = load meth db arch, None in
    match comp with
    | None -> [default]
    | Some comp -> [default; load ~comp meth db arch, Some comp] in
  List.map bws ~f:(fun (bw,comp) ->
      bw >>= fun bw ->
      Table.iteri (Image.segments img) ~f:(fun mem sec ->
          if Image.Segment.is_executable sec then
            BW.train bw ~max_length:length test mem);
      let data = Binable.to_string (module BW) bw in
      Sigs.save ?comp ~mode:"bytes" ~path:db arch (Bytes.of_string data) |>
  Result.map_error ~f:(fun e ->
      Error.createf "signatures are not updated: %s" @@
      Sigs.string_of_error e)) |> Result.all_unit

let matching =
  let open FileUtil in
  let ignored =
    List.map ignored ~f:Re_posix.re |> Re.alt |> Re.compile in
  let matches s = Re.execp ignored s in
  let ignored = Custom matches in
  FileUtil.(And (Is_file, Not ignored))
[@@warning "-D"]

let train meth length comp db paths =
  let db = Option.value db ~default:"sigs.zip" in
  let collect path =
    FileUtil.find matching path (fun xs x -> x :: xs) [] in
  let files = List.map paths
      ~f:(fun f -> if Sys.is_directory f then collect f else [f]) |>
              List.concat |> List.sort ~compare:String.compare in
  let total = List.length files in
  let start = Sys.time () in
  let errors = ref 0 in
  List.iteri files ~f:(fun n path ->
      printf "[%3d / %3d] %-66s%!" (n+1) total path;
      let r = match train_on_file comp meth length db path with
        | Ok () -> "OK"
        | Error err ->
          eprintf "Error: %a\n%!" Error.pp err;
          (incr errors); "SKIPPED" in
      printf "%6s\n%!" r);
  printf "Processed %d files out of %d in %g seconds\n"
    (total - !errors) total (Sys.time () -. start);
  printf "Signatures are stored in %s\n%!" db;
  Ok ()

let create_bw comp img path =
  let arch = Image.arch img in
  Sigs.load ?comp ?path ~mode:"bytes" arch |>
  Result.map_error ~f:(fun e ->
      Error.createf "failed to read signatures from a database: %s"
        (Sigs.string_of_error e)) >>| fun data ->
  Binable.of_string (module BW) (Bytes.to_string data)

let find threshold length comp path input =
  Image.create input >>= fun (img,_warns) ->
  create_bw comp img path >>= fun bw ->
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

let dump comp info length threshold path (input : string) =
  Image.create input >>= fun (img, _warns) ->
  match info with
  | `BW ->
    create_bw comp img path >>= fun bw ->
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
  | Curl.CurlException (err,_n,_) ->
    Or_error.errorf "failed to fetch: %s" (Curl.strerror err)
  | exn -> Or_error.of_exn ~backtrace:`Get exn

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
  open Cmdliner

  let filename : string Term.t =
    let doc = "Input filename." in
    Arg.(required & pos 0 (some non_dir_file) None &
         info [] ~doc ~docv:"FILE")

  let database_in : string option Term.t =
    let doc = "Path to signature database" in
    Arg.(value & opt (some non_dir_file) None &
         info ["db"; "d"] ~doc)

  let database : string option Term.t =
    let doc = "Update or create database at $(docv)" in
    Arg.(value & opt (some string) None &
         info ["db"; "d"] ~doc ~docv:"DBPATH")

  let files : string list Term.t =
    let doc = "Input files and directories" in
    Arg.(non_empty & pos_all file [] & info [] ~doc ~docv:"FILE")

  let compiler : string option Term.t =
    let doc = "Assume the training set is compiled by $(docv)" in
    Arg.(value & opt (some string) None &
         info ["comp"] ~doc ~docv:"COMPILER")

  let length : int Term.t =
    let doc = "Maximum prefix length" in
    Arg.(value & opt int 16 & info ["length"; "l"] ~doc)

  let meth : [`update | `rewrite] Term.t =
    let enums = ["update", `update; "rewrite", `rewrite] in
    let doc = sprintf "If entry exists then %s it." @@
      Arg.doc_alts_enum enums in
    Arg.(value & opt (enum enums) `update &
         info ["method"; "m"] ~doc)

  let threshold : float Term.t =
    let doc = "Decide that sequence starts a function \
               if m / n > $(docv),  where n is the total \
               number of occurrences of a given sequence in \
               the training set, and m is how many times \
               it has occurred at the function start position." in
    Arg.(value & opt float 0.5 &
         info ["threshold"; "t"] ~doc ~docv:"THRESHOLD")

  let url : string Term.t =
    let doc = "Url of the binary signatures" in
    let default = sprintf
        "https://github.com/BinaryAnalysisPlatform/bap/\
         releases/download/v%s/sigs.zip" Config.version in
    Arg.(value & opt string default & info ["url"] ~doc)

  let src : string Term.t =
    Arg.(value & pos 0 non_dir_file "sigs.zip" & info []
           ~doc:"Signatures file" ~docv:"SRC")
  let dst : string Term.t =
    Arg.(value & pos 1 string Sigs.default_path &
         info [] ~doc:"Destination" ~docv:"DST")

  let output : string Term.t =
    let doc = "Output filename" in
    Arg.(value & opt string "sigs.zip" & info ["o"] ~doc)

  let print_name : bool Term.t =
    let doc = "Print symbol's name." in
    Arg.(value & flag & info ["print-name"; "n"] ~doc)

  let print_size : bool Term.t =
    let doc = "Print symbol's size." in
    Arg.(value & flag & info ["print-size"; "s"] ~doc)

  let tool : [`BW | `SymTbl] Term.t =
    let enums = ["byteweight", `BW; "symbols", `SymTbl] in
    let doc = sprintf "The info to be dumped. %s" @@ Arg.doc_alts_enum enums in
    Arg.(value & opt (enum enums) `BW & info ["info"; "i"] ~doc)

  let dump =
    let doc = "Dump the function starts in a given executable by given tool" in
    Term.(pure dump $compiler $tool $length $threshold $database_in $filename),
    Term.info "dump" ~doc

  let train =
    let doc = "Train byteweight on the specified set of files" in
    Term.(pure train $meth $length $compiler $database $files),
    Term.info "train" ~doc

  let find =
    let doc = "Output all function starts in a given executable" in
    Term.(pure find $threshold $length $compiler $database_in $filename),
    Term.info "find" ~doc

  let fetch =
    let doc = "Fetch signatures from the specified url" in
    Term.(pure fetch $output $url), Term.info "fetch" ~doc

  let install =
    let doc = "Install signatures" in
    Term.(pure install $src $dst), Term.info "install" ~doc

  let update =
    let doc = "Download and install latest signatures" in
    Term.(pure update $url $dst), Term.info "update" ~doc

  let symbols =
    let doc = "Print file's symbol table if any." in
    Term.(pure symbols $print_name $print_size $filename),
    Term.info "symbols" ~doc

  let usage choices =
    eprintf "usage: bap-byteweight [--version] [--help] \
             <command> [<args>]\n";
    eprintf "where <command> := %s.\n"
      (String.concat ~sep:" | " choices);
    Ok ()

  let default =
    let doc = "byteweight toolkit" in
    let man = [
      `S "DESCRIPTION";
      `P "A toolkit for training, fetching, testing
          and installing byteweight signatures.";
      `S "SEE ALSO";
      `P "$(b,bap)(1), $(b,bap-plugin-byteweight)(1)"
    ] in
    Term.(pure usage $ choice_names),
    Term.info "bap-byteweight"
      ~version:Config.version ~doc ~man

  let eval argv = Term.eval_choice ~argv default
      [train; find; fetch; install; update; symbols; dump]
end

let () =
  Log.start ();
  let args = Bap_plugin_loader.run ["byteweight-frontend"] Sys.argv in
  match Cmdline.eval args with
  | `Ok Ok () -> ()
  | `Ok Error err ->
    eprintf "Program failed: %s\n%!"
      Error.(to_string_hum err);
    exit 2
  | `Version | `Help -> ()
  | _ -> exit 1
