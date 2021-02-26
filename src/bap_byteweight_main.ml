let man = {|
# DESCRIPTION

A utitilty to manipulate Byteweight function start signatures. This
utility is not needed for Byteweight to function properly, for that
you need the byteweight plugin. The utility is used to update
signatures, using new training corpora, fetch the existing signatures
from the Internet, install it into proper locations, and test the
results.

# TRAINING

Signatures are stored in an archive, which is managed by the
bap-byteweight library. The archive consists of several independent
entries, for each combination of an architecture, compiler, and matching
mode, having the following name $(b,<arch>/<comp>/<mode>). The
$(b,default) name is used as a catch all compiler. The only mode
currently supported is the $(b,byte) mode (i.e., matching on bytes).

Each entry is a prefix tree, in which each prefix is a substring that
was ever seen in the training corpora as a function start. Each prefix
is associated with a pair of values, $(b,a) - the number of occurences
of the prefix as a function start, and $(b,b) - the number of
occurences of the prefix as not a function start.

During the training, if the operaton mode is set to $(b,update) (the
default), then the existing prefixed will be updated. The oracle, that
tells which prefix starts a function or not, is derived from the
binary itself. Therefore the training corpora should have non-empty
symbol tables (and when applicable DWARF information). The
$(b,symbols) command will output the set of addresses that will be
used for training.

When the operation mode is set to $(b,rewrite) then each round of
training (i.e., each input file) will create a new entry for the given
architecture, compiler, matching mode triple. So, probably this is not
what you're looking for.

# THE IDENTIFICATION PROCEDURE

The input memory is scanned, and for each byte that is not yet
classified as a function start the longest sequence of bytes is
searched in the signatures. If one is found, then the $(b,threshold)
parameter defines the decision procedure. If it is a value below
$(b,1.0) then the sequence of bytes will be classified as a function
start if the the associated probability is higher than the specified
threshold.  If the threshold is greater or equal than 1.0, then the
sequence of bytes will be classified as a function start if the
Bayes factor of the two competing hypothesis is greater than the
specified threshold. The Bayes factor is the ratio between the
posterior probabilities of the competing hypothesis. Therefore, it
includes the prior odds of finding a function start, which makes the
hypothesis testing more robust. The Bayes factor value is having the
following interpretations:

```
    Bayes Factor          Strength

    1 to 3.2              Weak
    3.2 to 10             Substantial
    10 to 100             Strong
    100 and greater       Decisive;
```
|}

open Core_kernel
open Bap.Std
open Result.Monad_infix
open Format
open Bap_main

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures
module Digest = Caml.Digest
module Config = Extension.Configuration

type failure =
  | Image of Error.t
  | Sigs of Sigs.error
  | Network of Curl.curlCode * int * string
  | Http of int
  | Unknown of exn

type Extension.Error.t += Byteweight_failed of failure

let github =
  sprintf "https://github.com/BinaryAnalysisPlatform/bap/\
           releases/download/v%s/sigs.zip"

module Filename = Stdlib.Filename

(** list of posix regexes of files that are skipped during
    the training.  *)
let ignored = [
  ".*\\.git.*";
  ".*/README.*";
  sprintf ".*\\.(%s)" @@ String.concat ~sep:"|" [
    "txt"; "html"; "htm"; "md"; "py"; "pyc"; "sh"
  ];
] |> List.map ~f:Re.Posix.re |> Re.alt |> Re.compile

let bw_failure err = Byteweight_failed err
let fail err = Error (bw_failure err)

let create_image loader input =
  Image.create ~backend:loader input |>
  Result.map_error ~f:(fun err -> bw_failure (Image err)) >>|
  fst

let with_sigs_error = Result.map_error ~f:(fun err ->
    bw_failure (Sigs err))

let load_or_create_signatures ?comp ?path operation arch =
  match operation with
  | `rewrite -> Ok (BW.create ())
  | operation ->
    match Sigs.load ?comp ?path ~mode:"bytes" arch, operation with
    | Ok s,_ -> Ok (Binable.of_string (module BW) (Bytes.to_string s))
    | Error (`No_entry _|`No_signatures), `update ->
      Ok (BW.create ())
    | Error e,_ -> fail (Sigs e)

let oracle_of_image img =
  let starts = Hash_set.create (module Addr) in
  Image.symbols img |> Table.iteri ~f:(fun mem _ ->
      Hash_set.add starts (Memory.min_addr mem));
  fun mem -> Hash_set.mem starts (Memory.min_addr mem)

let code_of_image img =
  Image.memory img |> Memmap.to_sequence |>
  Seq.filter_map ~f:(fun (mem,desc) ->
      Option.some_if (Value.is Image.code_region desc) mem)

let train_on_file loader comp operation max_length db path =
  create_image loader path >>= fun img ->
  let arch = Image.arch img in
  let oracle = oracle_of_image img in
  load_or_create_signatures ?comp ~path:db operation arch >>= fun bw ->
  Seq.iter (code_of_image img) ~f:(BW.train bw ~max_length oracle);
  with_sigs_error @@
  Sigs.save ?comp ~mode:"bytes" ~path:db arch @@
  Bytes.of_string @@
  Binable.to_string (module BW) bw

let add_file files path =
  Map.set files ~key:(Digest.file path) ~data:path

let all_not_ignored_files =
  FileUtil.(And (Is_file, Not (Custom (Re.execp ignored))))

let add_path files path =
  if Sys.is_directory path
  then FileUtil.find all_not_ignored_files path add_file files
  else add_file files path

let train loader operation length comp db paths _ctxt =
  let db = Option.value db ~default:"sigs.zip" in
  let init = Map.empty (module String) in
  let files = List.fold ~init paths ~f:add_path in
  let total = Map.length files in
  let errors = ref 0 in
  let step = ref 0 in
  Map.iter files ~f:(fun path ->
      incr step;
      printf "[%3d / %3d] %-66s%!" !step total path;
      match train_on_file loader comp operation length db path with
      | Ok () -> printf "%6s\n%!" "OK"
      | Error err ->
        incr errors;
        printf "%6s\n%!" "SKIPPED";
        eprintf "Error: %a\n%!" Extension.Error.pp err;
        (incr errors));
  printf "Signatures were stored in %s\n%!" db;
  Ok ()

let find loader threshold min_length max_length comp path input _ctxt =
  create_image loader input >>= fun img ->
  let arch = Image.arch img in
  load_or_create_signatures ?comp ?path `load arch >>| fun bw ->
  let find = if Float.(threshold > 1.)
    then BW.find_using_bayes_factor bw threshold
        ~min_length ~max_length
    else BW.find_using_threshold bw threshold
        ~min_length ~max_length in
  let scan_memory mem =
    find mem |> List.iter ~f:(fun addr ->
        printf "%a\n" Addr.pp addr) in
  code_of_image img |> Seq.iter ~f:scan_memory

let symbols loader print_name print_size input _ctxt =
  create_image loader input >>| Image.symbols >>| fun syms ->
  Table.iteri syms ~f:(fun mem sym ->
      printf "%a" Addr.pp (Memory.min_addr mem);
      if print_name
      then printf " %s" (Image.Symbol.name sym);
      if print_size
      then printf " %4d" (Memory.length mem);
      printf "\n")


module SymIO = struct
  let read arch ic : (string * addr * addr) list =
    let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
    let addr_of_int64 x =
      let width = Arch.addr_size arch |> Size.in_bits in
      Addr.of_int64 ~width x in
    List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
        s, addr_of_int64 es, addr_of_int64 ef))

  let read_addrs ic : addr list =
    List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

  let write_addrs oc (addrs : addr list) : unit =
    Sexp.output oc @@ List.sexp_of_t Addr.sexp_of_t addrs

  let write oc (syms : (string * addr * addr) list) : unit =
    let sexp_of_sym x = [%sexp_of:string * int64 * int64] x in
    try
      let syms = List.map syms ~f:(fun (s, es, ef) -> s, Addr.to_int64 es |> ok_exn,
                                                      Addr.to_int64 ef |> ok_exn) in
      List.iter syms ~f:(fun sym -> Sexp.output_hum oc @@ sexp_of_sym sym;
                          Out_channel.output_char oc '\n')
    with exn ->
      printf "Output error: %a." Exn.pp exn;
      ()
end

let dump loader comp info min_length max_length threshold path input _ctxt =
  create_image loader input >>= fun img ->
  let arch = Image.arch img in
  match info with
  | `BW ->
    load_or_create_signatures ?comp ?path `load arch >>| fun bw ->
    let find = if Float.(threshold > 1.)
      then BW.find_using_bayes_factor bw threshold
          ~min_length ~max_length
      else BW.find_using_threshold bw threshold
          ~min_length ~max_length in

    SymIO.write_addrs stdout @@ Addr.Set.to_list @@
    Seq.fold (code_of_image img)
      ~init:Addr.Set.empty ~f:(fun starts mem ->
          List.fold (find mem) ~init:starts ~f:Set.add)
  | `SymTbl ->
    let syms = Image.symbols img in
    let mems = Table.regions syms in
    SymIO.write_addrs stdout @@ Seq.to_list (Seq.map mems Memory.min_addr);
    Ok ()

let create_parent_dir dst =
  let dir = if Filename.(check_suffix dst dir_sep)
    then dst else Filename.dirname dst in
  FileUtil.mkdir ~parent:true dir

let fetch fname url version _ctxt =
  let url = match version with
    | None -> url
    | Some v -> github v in
  let tmp,fd = Filename.open_temp_file "bap_" ".sigs" in
  let write s = Out_channel.output_string fd s; String.length s in
  let conn = Curl.init () in
  Curl.set_writefunction conn write;
  Curl.set_followlocation conn true;
  Curl.set_sslverifypeer conn false;
  Curl.set_url conn url;
  let result = try Ok (Curl.perform conn) with
    | Curl.CurlException (code,n,str)-> fail (Network (code,n,str))
    | exn -> fail (Unknown exn) in
  let code = Curl.get_httpcode conn in
  Curl.cleanup conn;
  Out_channel.close fd;
  match result with
  | Ok () when code < 300 ->
    create_parent_dir fname;
    FileUtil.mv tmp fname;
    printf "Successfully downloaded to %s\n" fname;
    Ok ()
  | _ ->
    FileUtil.rm [tmp];
    if Result.is_ok result
    then fail (Http code)
    else result

let install src dst _ctxt =
  try
    create_parent_dir dst;
    FileUtil.cp [src] dst;
    printf "Installed signatures to %s\n" dst;
    Ok ()
  with exn -> fail (Unknown exn)

let update url dst version ctxt =
  let old = Filename.temp_file "bap_old_sigs" ".zip" in
  if Sys.file_exists dst
  then FileUtil.cp [dst] old;
  fetch dst url version ctxt >>| fun () ->
  if Option.is_none @@ FileUtil.cmp old dst
  then printf "Already up-to-date.\n"
  else printf "Installed new signatures.\n";
  if Sys.file_exists old then FileUtil.rm [old]

module Parameters = struct
  open Extension

  let filename = Command.argument Type.non_dir_file
      ~doc:"Input filename."

  let loader = Command.parameter Type.(string =? "llvm") "loader"
      ~doc:"Loader for binary files"

  let database_in = Command.parameter Type.(some non_dir_file) "db"
      ~aliases:["d"]
      ~doc:"Path to signature database"

  let database = Command.parameter Type.(some string) "signatures"
      ~doc:"Update or create database at $(docv)"
      ~aliases:["d"; "db"; "sigs"]

  let files = Command.arguments Type.file
      ~doc:"Input files and directories"

  let compiler = Command.parameter Type.(some string) "comp"
      ~doc:"Assume the training set is compiled by $(docv)"

  let min_length = Command.parameter Type.(int =? 8) "min-length"
      ~doc:"The minimum length of a word, that could identify a \
            function start. Any signatures that are below that \
            length, will not be considered, affect prior \
            probabilities, etc."

  let max_length = Command.parameter Type.(int =? 16) "max-length"
      ~aliases:["l"; "length"]
      ~doc:"The maximum length of a word, that could identify a \
            function start. Any signatures that are greater than that \
            length, will not be considered, affect prior \
            probabilities, etc."


  let operations = Type.enum [
      "update", `update;
      "rewrite", `rewrite
    ]

  let operation = Command.parameter operations "method"
      ~doc:"If entry exists then 'update' or 'rewrite' it."
      ~aliases:["m"; "operation"]

  let threshold = Command.parameter Type.(float =? 10.) "threshold"
      ~doc:"If greater than 1.0 then it is the Bayes factor, \
            otherwise it is a probability."
      ~aliases:["t"]

  let ver = Configuration.version
  let url = Command.parameter Type.(string =? github ver) "url"
      ~doc:"URL of the function starts signatures."

  let version = Command.parameter Type.(some string) "from-version"
      ~aliases:["v"; "for-version"]
      ~doc:"fetch signatures for the specified version"

  let src = Command.argument Type.("SRC" %: non_dir_file =? "sigs.zip")
      ~doc:"Signatures file"

  let dst =
    Command.argument Type.(path =? Sigs.default_path)
      ~doc:"Destination"

  let output = Command.parameter Type.(string =? "sigs.zip") "o"
      ~doc:"Output filename"

  let print_name = Command.flag "print-name"
      ~doc:"Print symbol's name."
      ~aliases:["n"]

  let print_size = Command.flag "print-size"
      ~doc:"Print symbol's size."
      ~aliases:["s"]

  let tools = Type.enum [
      "byteweight", `BW;
      "symbols", `SymTbl
    ]

  let tool = Command.parameter tools "info"
      ~doc:"The info to be dumped"
      ~aliases:["i"]

end
let () =
  let open Extension.Command in
  let open Parameters in
  declare "dump"
    (args $loader $compiler $tool $min_length $max_length $threshold $database_in $filename)
    dump
    ~doc:"Dumps the function starts in machine readable format.";
  declare "train"
    (args $loader $operation $max_length $compiler $database $files)
    train
    ~doc:"Trains on the specified set of files.";
  declare "find"
    (args $loader $threshold $min_length $max_length $compiler $database_in $filename)
    find
    ~doc:"Outputs the function starts detected with Byteweight.";
  declare "fetch"
    (args $output $url $version)
    fetch
    ~doc:"Downloads signatures from to the current folder.";
  declare "install"
    (args $src $dst)
    install
    ~doc:"Copies signatures to the predefined location.";
  declare "update"
    (args $url $dst $version)
    update
    ~doc:"Downloads and installs signatures.";
  declare "symbols"
    (args $loader $print_name $print_size $filename)
    symbols
    ~doc:"Outputs the function starts provided by the binary (ground truth)."
let string_of_error = function
  | Image err ->
    Format.asprintf "Failed to load a file: %a" Error.pp err
  | Sigs err ->
    sprintf "Failed to load/store signatures: %s"
      (Sigs.string_of_error err)
  | Network (err,code,msg) ->
    sprintf "Network failure: %s, %d, %s" (Curl.strerror err) code msg
  | Unknown exn ->
    sprintf "An unexpected exception: %s"
      (Exn.to_string exn)
  | Http code ->
    sprintf "got an unexepected HTTP code %d" code

let () = Extension.Error.register_printer @@ function
  | Byteweight_failed err -> Some (string_of_error err)
  | _ -> None

let default ctxt =
  print_endline {|
Usage:
  bap-byteweight <COMMAND> [<OPTIONS>]
Commands:
|};
  let open Extension.Configuration in
  List.iter (commands ctxt) ~f:(fun cmd ->
      printf "  %-24s %s\n"
        (info_name cmd)
        (info_doc cmd));
  Ok ()

let () =
  match Bap_main.init ()
          ~features:["byteweight-frontend"]
          ~requires:["loader"; "byteweight"]
          ~argv:Sys.argv
          ~man
          ~default
          ~name:"bap-byteweight"
  with Ok () -> ()
     | Error err ->
       Format.eprintf "Program failed with: %a@\n%!"
         Extension.Error.pp err
