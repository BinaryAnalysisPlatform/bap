open Core_kernel.Std
open Bap_ida.Std
open Bap.Std

include Self ()

type ida_kind = [ `idal | `idal64 | `idaq | `idaq64 ] [@@deriving sexp]

type ida = {
  ida : string;
  exe : string;
  curses : string option;
  mutable trash : string list;
  debug : int;
} [@@deriving sexp]

module Ida_config = struct
  type t = {
    ida_path : string;
    ida_kind : ida_kind option;
    curses : string option;
    debug  : int;
  }
end
type config = Ida_config.t

type 'a command = 'a Command.t

let ext = FilePath.replace_extension

let ida_of_suffix filename =
  let exists suf = Sys.file_exists (ext filename suf) in
  if exists "i64" then  Some `idaq64
  else if exists "idb" then Some `idaq
  else None

let find_ida target path kind =
  let kind = match kind with
    | Some kind -> kind
    | None -> match ida_of_suffix target with
      | Some ida -> ida
      | None -> `idaq64 in
  let s = Sexp.to_string (sexp_of_ida_kind kind) in
  let ida = Filename.concat path s in
  Filename.quote ida

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let system cmd =
  if Sys.command cmd <> 0 then raise (Ida.Failed cmd)
let pread cmd = ksprintf run cmd
let shell cmd = ksprintf (fun cmd () -> system cmd) cmd

let idb ida p =
  if Filename.check_suffix ida "64"
  then ext p "i64" else ext p "idb"

let asm p = ext p "asm"

(* Headless IDA on linux systems dlopens libcurses.so (sic).
   So, we need to find a 32-bit libcurses library, copy it
   to a temporary folder renaming to `libcurses.so`, as it
   can have different suffixes and prefixes. *)
let setup_headless_env path =
  let lib = Filename.temp_file "bap_" "lib" in
  FileUtil.rm [lib];
  FileUtil.mkdir lib;
  FileUtil.cp [path] (Filename.concat lib "libcurses.so");
  let var = "LD_LIBRARY_PATH" in
  let old_path,new_path =
    try
      Unix.getenv var, lib ^ ":" ^ Unix.getenv var
    with Not_found -> "", lib in
  Unix.putenv var new_path;
  fun () ->
    try
      FileUtil.rm ~recurse:true [lib];
      if old_path <> "" then Unix.putenv var old_path
    with _ -> ()

let cleanup_minidump () =
  let is_dump x = Filename.check_suffix x ".dmp" in
  let dump_path = "/tmp/ida" in
  if Sys.file_exists dump_path && Sys.is_directory dump_path then
    FileUtil.ls dump_path  |>
    List.filter ~f:is_dump |> function
    | [] -> ()
    | files ->
      info "ida minidump is not empty";
      let lock = sprintf "%s/lock" dump_path in
      let lock = Unix.openfile lock Unix.[O_RDWR; O_CREAT] 0o666 in
      Unix.lockf lock Unix.F_LOCK 0;
      protect ~f:(fun () ->
          List.iter files ~f:Sys.remove)
        ~finally:(fun () ->
            Unix.lockf lock Unix.F_ULOCK 0;
            Unix.close lock)

(* ida works fine only if everything is in the same folder  *)
let run (t:ida) cmd =
  cleanup_minidump ();
  let cwd = Unix.getcwd () in
  let clean = match t.curses with
    | Some path -> setup_headless_env path
    | None -> fun () -> () in
  let cleanup () = clean (); Sys.chdir cwd in
  Sys.chdir (Filename.dirname t.exe);
  try cmd (); cleanup ()
  with exn -> cleanup (); raise exn

let check_path path = match Bap_ida_check.check_path path with
  | Ok () -> ()
  | Error e ->
    eprintf "failed to check ida path: %s." (Error.to_string_hum e);
    exit 1

let create {Ida_config.ida_path; ida_kind; debug; curses} target =
  if not (Sys.file_exists target)
  then invalid_argf "Can't find target executable" ();
  let exe = Filename.temp_file "bap_" "_ida" in
  FileUtil.cp [target] exe;
  let ida = find_ida target ida_path ida_kind in
  let idb = idb ida in
  let self = {
    ida;
    exe;
    curses;
    trash = [exe; idb exe; asm exe];
    debug;
  } in
  if Sys.file_exists (idb target) then (
    FileUtil.cp [idb target] (idb exe);
    if Sys.file_exists (asm target) then
      FileUtil.cp [asm target] (asm exe);
  ) else (
    check_path ida_path;
    run self @@ shell "%s -A -B %s" self.ida self.exe;
  );
  self

let substitute s (x,y) =
  let b = Buffer.create (String.length s + 64) in
  Buffer.add_substitute b (fun s -> if s = x then y else s) s;
  Buffer.contents b

let exec (self:ida) command =
  let ext = if Command.language command = `idc then ".idc" else ".py" in
  let script,out = Filename.open_temp_file "bap_" ext in
  let result = Filename.temp_file "bap_" ".ida_out" in
  substitute (Command.script command) ("output", result) |>
  Out_channel.output_string out;
  Out_channel.close out;
  self.trash <- script :: result :: self.trash;
  let auto = if self.debug > 1 then "" else "-A" in
  run self @@ shell "%s %s -S%s %s" self.ida auto script self.exe;
  (Command.parser command) result

let close (self:ida) =
  if self.debug < 2 then
    FileUtil.rm self.trash

(* we're looking for libcurses library in loader database, excluding
   64-bit libraries.
*)
let find_curses () =
  let x86_64 = Re_posix.re ".*x86.64.*" |> Re.compile in
  let curses = Re_posix.re ".*lib.curses\\.so.*" |> Re.compile in
  pread "ldconfig -p" |> List.filter ~f:(Fn.non (Re.execp x86_64)) |>
  List.filter ~f:(Re.execp curses) |> List.filter_map ~f:(fun s ->
      match String.split ~on:'>' s with
      | [_;path] -> Some (String.strip path)
      | _ -> None) |> List.filter ~f:Sys.file_exists |> List.hd

let register ida_path ida_kind is_headless : unit =
  let curses = if Sys.os_type = "Unix" && is_headless
    then find_curses () else None in
  let debug =
    try Int.of_string (Sys.getenv "BAP_IDA_DEBUG") with _exn -> 0 in
  let config = {
    Ida_config.ida_path;
    ida_kind;
    curses;
    debug
  } in
  let create (target:string) : Service.t =
    let self = create config target in
    let exec cmd = exec self cmd in
    let close () = close self in
    Service.{ exec; close } in
  Service.provide create
