open Core_kernel
open Bap.Std
include Self()

type version = Vold | Vnew [@@deriving sexp]
type old_kind = [ `idal | `idal64 | `idaq | `idaq64 ] [@@deriving sexp, enumerate]
type new_kind = [ `idat | `idat64 | `ida  | `ida64  ] [@@deriving sexp, enumerate]
type ida_kind = [ old_kind | new_kind ] [@@deriving sexp]

type ida = {
    headless  : ida_kind;
    graphical : ida_kind;
    headless64  : ida_kind;
    graphical64 : ida_kind;
    version : version;
  } [@@deriving sexp]

type t = {
    path  : string;
    ida   : ida;
    is_headless : bool;
  }

type mode = [ `m32 | `m64 ]

type failure =
  | Ida_not_exists
  | Non_dir_path
  | File_not_found of string
  | Unexpected_kind
  | Headless_win32
  | Ida_python_missed

let (/) = Filename.concat

let string_of_kind k = Sexp.to_string (sexp_of_ida_kind k)

let code_of_failure = function
  | Ida_not_exists   -> 0
  | Non_dir_path     -> 1
  | File_not_found _ -> 2
  | Unexpected_kind  -> 3
  | Headless_win32   -> 4
  | Ida_python_missed-> 5

let string_of_failure = function
  | Ida_not_exists -> "IDA path not exists"
  | Non_dir_path -> "IDA path is not a directory"
  | File_not_found k -> k ^ " must exist"
  | Unexpected_kind -> "can't infer IDA version. Check IDA installation"
  | Headless_win32 -> "can't use headless on windows"
  | Ida_python_missed -> "bap-ida-python is not installed"

module Check = struct

  type error = (unit, failure) Result.t

  let require = Result.ok_if_true

  let require_path path = require (Sys.file_exists path)
  let require_dir path = require (Sys.is_directory path) ~error:Non_dir_path

  let require_ida path =
    let open Result in
    require_path path ~error:Ida_not_exists >>= fun () ->
    require_dir path

  let require_ida_python path =
    require_path ~error:Ida_python_missed
      (path / "plugins" / "plugin_loader_bap.py")

  let require_kind path k =
    let file = path / string_of_kind k in
    require_path ~error:(File_not_found file) file

  let run {path; ida} =
    let require_kind = require_kind path in
    Result.all_ignore [
        require_ida path;
        require_ida_python path;
        require_kind ida.graphical;
        require_kind ida.graphical64;
        require_kind ida.headless;
        require_kind ida.headless64; ]

  let check_integrity ida =
    let files = [ida.graphical; ida.headless;
                 ida.graphical64; ida.headless64;] in
    let is_of kinds = List.mem (kinds :> ida_kind list) ~equal:(=) in
    let check version = List.for_all files ~f:(is_of version) in
    let expected = match ida.headless with
      | `idal -> (all_of_old_kind :> ida_kind list)
      | _ -> (all_of_new_kind :> ida_kind list) in
    let checked = check expected in
    require checked ~error:Unexpected_kind

  let check_headless is_headless =
    require (is_headless ==> not Sys.win32) ~error:Headless_win32

end

let check ida =
  match Check.run ida with
  | Ok () as ok -> ok
  | Error fail ->
     Or_error.errorf "IDA check failed with error code %d"
       (code_of_failure fail)

let exists_kind path kind =
  Sys.file_exists (path / string_of_kind kind)

let create_ida path =
  let find_kind ~is_headless mode =
    let kinds = match mode with
      | `m32 when is_headless -> [`idal; `idat]
      | `m64 when is_headless -> [`idal64; `idat64]
      | `m32 -> [`idaq; `ida]
      | `m64 -> [`idaq64; `ida64] in
    List.find ~f:(exists_kind path) kinds |>
      function
      | Some k -> Ok k
      | None ->
         let kinds = List.map ~f:string_of_kind kinds in
         let files = String.concat ~sep:"/" kinds in
         Error (File_not_found files) in
  let version_of_headless = function
    | `idal -> Vold
    | _ -> Vnew in
  Result.(
    find_kind ~is_headless:true  `m32 >>= fun headless ->
    find_kind ~is_headless:false `m32 >>= fun graphical ->
    find_kind ~is_headless:true  `m64 >>= fun headless64 ->
    find_kind ~is_headless:false `m64 >>= fun graphical64 ->
    let version = version_of_headless headless in
    Ok { headless; graphical; headless64; graphical64; version })

let create' path is_headless =
  let open Check in
  Result.(
    require_ida path >>= fun () ->
    require_ida_python path >>= fun () ->
    check_headless is_headless >>= fun () ->
    create_ida path >>= fun ida ->
    check_integrity ida >>= fun () ->
    Ok {ida; path; is_headless})

let create path is_headless =
  match create' path is_headless with
  | Ok r -> Ok r
  | Error fail ->
     warning "%s" (string_of_failure fail);
     Or_error.errorf "IDA detection failed with error code %d" (code_of_failure fail)

(* Note, we always launch headless ida in case of IDA Pro 7 *)
let ida32 info = match info.ida.version with
  | Vnew -> info.ida.headless
  | Vold ->
     if info.is_headless then info.ida.headless
     else info.ida.graphical

let ida64 info = match info.ida.version with
  | Vnew -> info.ida.headless64
  | Vold ->
     if info.is_headless then info.ida.headless64
     else info.ida.graphical64

let ida_of_suffix info filename =
  let ext = FilePath.replace_extension in
  let exists suf = Sys.file_exists (ext filename suf) in
  if exists "i64" then  Some (ida64 info)
  else if exists "idb" then Some (ida32 info)
  else None

let ida_of_mode info = function
  | `m32 -> ida32 info
  | `m64 -> ida64 info

let find_ida info mode target =
    let kind = match mode with
      | Some mode -> ida_of_mode info mode
      | None ->
         match ida_of_suffix info target with
         | Some ida -> ida
         | None -> ida64 info in
    let s = Sexp.to_string (sexp_of_ida_kind kind) in
    let ida = Filename.concat info.path s in
    Filename.quote ida

let is_headless t = t.is_headless
let path t = t.path

let require_ncurses t =
  Sys.os_type = "Unix" && t.is_headless && t.ida.version = Vold
