open Core_kernel.Std
open Bap.Std
open Word_size

type t = {
  ida : string;
  exe : string;
  curses : string option;
  mutable close : unit -> unit;
}

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let system cmd = Unix.system cmd |> ignore
let pread cmd = Printf.ksprintf run cmd
let shell cmd = Printf.ksprintf (fun cmd () -> system cmd) cmd


let idb,asm =
  let ext p e = FilePath.add_extension p e in
  let idb p = match word_size with
    | W32 -> ext p "idb"
    | W64 -> ext p "i64" in
  let asm p = ext p "asm" in
  idb,asm

let extract_symbols output = sprintf "
from idautils import *
with open('%s', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%%s 0x%%x 0x%%x)\\n' %% (
                GetFunctionName(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))

idc.Exit(0)" output

(* Headless IDA on linux systems dlopens libcurses.so (sic).
   So, we need to find a 32-bit libcurses library, copy it
   to a temporary folder renaming to `libcurses.so`, as it
   can have different suffixes and prefixes. *)
let setup_env path =
  let lib = Filename.temp_file "bap_" "lib" in
  FileUtil.rm [lib];  (* yep, this is a vulnerability *)
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
    if old_path <> "" then
      Unix.putenv var old_path

(* ida works fine only if everything is in the same folder  *)
let run t cmd =
  let cwd = Unix.getcwd () in
  let clean = match t.curses with
    | Some path -> setup_env path
    | None -> fun () -> () in
  Sys.chdir (FilePath.dirname t.exe);
  cmd ();
  clean ();
  Sys.chdir cwd

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

let locate exe =
  let first_found =
    pread "locate %s" exe |>
    List.filter ~f:Sys.file_exists |> List.hd in
  match first_found with
  | Some path -> path
  | None -> exe

let find exe = try FileUtil.which exe with Not_found -> locate exe

let is_headless = function
  | Some ida ->
    Re.execp (Re_posix.re ".*/?idal(64)?" |> Re.compile) ida
  | None -> false

let create_exn ?ida target =
  let curses = if Config.system = "linux" && is_headless ida
    then find_curses () else None in
  let ida = match ida with
    | Some path -> path
    | None when Sys.win32 ->
      failwithf "Don't know how to find files in Windows" ()
    | None -> match word_size with
      | W32 -> find "idaq"
      | W64 -> find "idaq64" in
  let ida =
    if Filename.is_implicit ida then find ida else ida in
  if not (Sys.file_exists ida) then raise Not_found;
  if not (Sys.file_exists target)
  then invalid_argf "Can't find target executable" ();
  let exe = Filename.temp_file "bap_" "_ida" in
  FileUtil.cp [target] exe;

  let self = {
    ida = Filename.quote ida;
    exe;
    curses;
    close = fun () ->
      FileUtil.rm [exe; idb exe; asm exe];
  } in

  if Sys.file_exists (idb target) then (
    FileUtil.cp [idb target] (idb exe);
    if Sys.file_exists (asm target) then
      FileUtil.cp [asm target] (asm exe);
  ) else (
    run self @@ shell "%s -A -B %s" self.ida self.exe;
  );
  self

let create ?ida target =
  try Ok (create_exn ?ida target) with
  | Not_found -> Or_error.errorf "Can't find IDA in your environment"
  | exn -> Or_error.of_exn exn

let run_script self script_to =
  let script,out = Filename.open_temp_file "bap_" ".py" in
  let result = Filename.temp_file "bap_" ".scm" in
  Out_channel.output_string out (script_to result);
  Out_channel.close out;
  run self @@ shell "%s -A -S%s %s" self.ida script self.exe;
  let close = self.close in
  self.close <- (fun () ->
      close ();
      FileUtil.rm [script; result]);
  result

let get_symbols ?demangle t arch mem =
  let result = run_script t extract_symbols in
  Symbols.read ?demangle ~filename:result arch  mem

let close self = self.close ()

let with_file ?ida target analysis =
  let open Or_error in
  create ?ida target >>= fun ida ->
  let result = try_with (fun () -> analysis ida) in
  close ida;
  result
