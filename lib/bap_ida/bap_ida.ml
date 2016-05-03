open Core_kernel.Std
open Word_size


type ida = {
  ida : string;
  exe : string;
  curses : string option;
  mutable trash : string list;
} [@@deriving sexp]

type 'a command = {
  script  : string;
  process : string -> 'a;
}

module Command = struct
  type 'a t = 'a command
  let create ~script ~process = {script; process}
end

module Ida = struct
  type t = ida

  exception Failed of string
  exception Not_in_path

  let run cmd =
    let inp = Unix.open_process_in cmd in
    let r = In_channel.input_lines inp in
    In_channel.close inp; r

  let system cmd =
    if Sys.command cmd <> 0 then raise (Failed cmd)
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
    Sys.chdir (Filename.dirname t.exe);
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

  let find exe =
    try FileUtil.which exe with Not_found -> exe

  let is_headless ida =
    Re.execp (Re_posix.re ".*/?idal(64)?" |> Re.compile) ida

  let find_ida ?ida () =
    let ida = match ida with
      | Some path -> path
      | None when Sys.win32 ->
        failwithf "Don't know how to find files in Windows" ()
      | None -> "idaq" in
    let ida = if Filename.is_implicit ida then find ida else ida in
    if Sys.file_exists ida then Some ida else None

  let exists ?ida () = find_ida ?ida () <> None

  let create ?ida target =
    if not (Sys.file_exists target)
    then invalid_argf "Can't find target executable" ();
    let ida = match find_ida ?ida () with
      | Some ida -> ida
      | None -> raise Not_in_path in
    let curses = if Sys.os_type = "Unix" && is_headless ida
      then find_curses () else None in
    let exe = Filename.temp_file "bap_" "_ida" in
    FileUtil.cp [target] exe;
    let idb = idb ida in
    let ida = Filename.quote ida in
    let self = {
      ida;
      exe;
      curses;
      trash = [exe; idb exe; asm exe]
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

  let exec self command =
    let script,out = Filename.open_temp_file "bap_" ".py" in
    let result = Filename.temp_file "bap_" ".ida_out" in
    substitute command.script ("output", result) |>
    Out_channel.output_string out;
    Out_channel.close out;
    self.trash <- script :: result :: self.trash;
    run self @@ shell "%s -A -S%s %s" self.ida script self.exe;
    command.process result

  let close self =
    FileUtil.rm self.trash

  let with_file ?ida target command =
    let ida = create ?ida target in
    let f ida = exec ida command in
    protectx ~f ida ~finally:close

  let get_symbols =
    Command.create
      ~script:"
from idautils import *
Wait()
with open('$output', 'w+') as out:
    for ea in Segments():
        fs = Functions(SegStart(ea), SegEnd(ea))
        for f in fs:
            out.write ('(%s 0x%x 0x%x)\\n' % (
                GetFunctionName(f),
                GetFunctionAttr(f, FUNCATTR_START),
                GetFunctionAttr(f, FUNCATTR_END)))

idc.Exit(0)"
      ~process:(In_channel.with_file ~f:(fun ch ->
          let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
          Sexp.input_sexps ch |> List.map ~f:blk_of_sexp))
end


module Std = struct
  type ida = Ida.t
  type 'a command = 'a Command.t
  module Ida = Ida
  module Command = Command
end
