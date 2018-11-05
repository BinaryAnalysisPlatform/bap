open Core_kernel

type version = V6 | V7 [@@deriving sexp]
type v6_kind = [ `idal | `idal64 | `idaq | `idaq64 ] [@@deriving sexp, enumerate]
type v7_kind = [ `idat | `idat64 | `ida  | `ida64  ] [@@deriving sexp, enumerate]
type ida_kind = [ v6_kind | v7_kind ] [@@deriving sexp]

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

let (/) = Filename.concat

let string_of_kind k = Sexp.to_string (sexp_of_ida_kind k)

let exists_kind path kind =
  Sys.file_exists (path / string_of_kind kind)

module Check = struct

  let require check req =
    if check then Ok ()
    else Or_error.errorf "IDA configuration failure: %s" req

  let check_headless is_headless =
    require (is_headless ==> not Sys.win32) "can't use headless on windows"

  let check_integrity ida =
    let files = [ida.graphical; ida.headless;
                 ida.graphical64; ida.headless64;] in
    let is_of kinds = List.mem (kinds :> ida_kind list) ~equal:(=) in
    let check version = List.for_all files ~f:(is_of version) in
    let checked = check all_of_v6_kind || check all_of_v7_kind in
    require checked "Ida files must stick to either 6th or 7th version"

  let run {path; ida;} =
    let exists = Sys.file_exists in
    let is_dir = Sys.is_directory in
    let exists_kind = exists_kind path in
    let msg_of_kind kind =
      sprintf "%s must exist" (string_of_kind kind) in
    Result.all_ignore [
        require (exists path) "path must exist";
        require (is_dir path) "path must be a folder";
        require (exists_kind ida.graphical)   @@ msg_of_kind ida.graphical;
        require (exists_kind ida.headless)    @@ msg_of_kind ida.headless;
        require (exists_kind ida.graphical64) @@ msg_of_kind ida.graphical64;
        require (exists_kind ida.headless64)  @@ msg_of_kind ida.headless64;
        require (exists (path / "plugins" / "plugin_loader_bap.py"))
          "bap-ida-python must be installed" ]
end

let check = Check.run

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
         let headless = if is_headless then "headless" else "" in
         let mode = match mode with | `m32 -> 32 | `m64 -> 64 in
         Or_error.errorf
           "Can't detect %s ida for %d mode" headless mode in
  let version_of_headless = function
    | `idal -> V6
    | _ -> V7 in
  Or_error.(
    find_kind ~is_headless:true  `m32 >>= fun headless ->
    find_kind ~is_headless:false `m32 >>= fun graphical ->
    find_kind ~is_headless:true  `m64 >>= fun headless64 ->
    find_kind ~is_headless:false `m64 >>= fun graphical64 ->
    let version = version_of_headless headless in
    Ok { headless; graphical; headless64; graphical64; version })

let create path is_headless =
  let open Check in
  Or_error.(
    check_headless is_headless >>= fun () ->
    create_ida path >>= fun ida ->
    check_integrity ida >>= fun () ->
    let info = {ida; path; is_headless} in
    check info >>= fun () ->
    Ok info)

(* Note, we always launch headless ida in case of IDA Pro 7 *)
let ida32 info = match info.ida.version with
  | V7 -> info.ida.headless
  | V6 ->
     if info.is_headless then info.ida.headless
     else info.ida.graphical

let ida64 info = match info.ida.version with
  | V7 -> info.ida.headless64
  | V6 ->
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
  Sys.os_type = "Unix" && t.is_headless && t.ida.version = V6
