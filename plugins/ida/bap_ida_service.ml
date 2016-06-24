open Core_kernel.Std
open Bap.Std
open Bap_ida.Std
open Word_size
open Result.Monad_infix

type ida = {
  ida : string;
  exe : string;
  curses : string option;
  mutable trash : string list;
  debug : int;
} [@@deriving sexp]

type config = {
  ida : string;
  curses : string option;
  idb : string -> string;
  debug : int;
}

type 'a command = 'a Command.t

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let system cmd =
  if Sys.command cmd <> 0 then raise (Ida.Failed cmd)
let pread cmd = ksprintf run cmd
let shell cmd = ksprintf (fun cmd () -> system cmd) cmd

let ext p e =
  FilePath.(add_extension (chop_extension p) e)

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
    FileUtil.rm ~recurse:true [lib];
    if old_path <> "" then Unix.putenv var old_path

(* ida works fine only if everything is in the same folder  *)
let run (t:ida) cmd =
  let cwd = Unix.getcwd () in
  let clean = match t.curses with
    | Some path -> setup_headless_env path
    | None -> fun () -> () in
  Sys.chdir (Filename.dirname t.exe);
  cmd ();
  clean ();
  Sys.chdir cwd

let create config target =
  if not (Sys.file_exists target)
  then invalid_argf "Can't find target executable" ();
  let exe = Filename.temp_file "bap_" "_ida" in
  FileUtil.cp [target] exe;
  let ida = config.ida in
  let curses = config.curses in
  let idb = config.idb in
  let debug = config.debug in
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

let require req check =
  if check
  then Ok ()
  else Or_error.errorf "IDA configuration failure: %s" req

let ida ida_path is_headless =
  let (/) = Filename.concat in
  require "path must exist"
    (Sys.file_exists ida_path) >>= fun () ->
  require "path must be a folder"
    (Sys.is_directory ida_path) >>= fun () ->
  require "can't use headless on windows"
    (is_headless ==> not Sys.win32) >>= fun () ->
  require "idaq exists"
    (Sys.file_exists (ida_path/"idaq")) >>= fun () ->
  require "idaq64 exists"
    (Sys.file_exists (ida_path/"idaq64")) >>= fun () ->
  require "idal exists"
    (Sys.file_exists (ida_path/"idal")) >>= fun () ->
  require "idal64 exists"
    (Sys.file_exists (ida_path/"idal64")) >>| fun () ->
  if is_headless
  then ida_path/"idal64"
  else ida_path/"idaq64"

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

let register ida_path is_headless : unit =
  let ida = ok_exn (ida ida_path is_headless) in
  let curses = if Sys.os_type = "Unix" && is_headless
    then find_curses () else None in
  let idb = idb ida in
  let ida = Filename.quote ida in
  let debug =
    try Int.of_string (Sys.getenv "BAP_IDA_DEBUG") with exn -> 0 in
  let config = {
    ida;
    curses;
    idb;
    debug
  } in
  let create (target:string) : Service.t =
    let self = create config target in
    let exec cmd = exec self cmd in
    let close () = close self in
    { exec; close } in
  Service.provide create
