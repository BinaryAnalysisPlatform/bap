(*  At the moment of writing, we have only two supprted version of IDA
 *  PRO: 6.9 and 7.1. And the only observable difference for us is
 *  a naming of executable files. Also, given that IDA version won't be
 *  change so fast we won't rely on IDA versions and will search for all
 *  possible files, for borh versions. *)

open Core_kernel

type v6 = [ `idal | `idal64 | `idaq | `idaq64 ] [@@deriving sexp, enumerate]
type v7 = [ `idat | `idat64 | `ida  | `ida64  ] [@@deriving sexp, enumerate]
type ida_kind = [ v6 | v7 ] [@@deriving sexp]

type ida = {
    headless  : ida_kind;
    graphical : ida_kind;
    headless64  : ida_kind;
    graphical64 : ida_kind;
  } [@@deriving sexp]

type t = {
    path  : string;
    ida   : ida;
    is_headless : bool;
  }

type mode = [ `m32 | `m64 ]

let (/) = Filename.concat

let string_of_kind k = Sexp.to_string (sexp_of_ida_kind k)

module Check = struct

  let require check req =
    if check
    then Ok ()
    else Or_error.errorf "IDA configuration failure: %s" req

  let check_headless is_headless =
    require (is_headless ==> not Sys.win32) "can't use headless on windows"

  let check_integrity ida =
    let files = [ida.graphical; ida.headless;
                 ida.graphical64; ida.headless64;] in
    let is_of xs x = List.mem (xs :> ida_kind list) x ~equal:(=) in
    let check x = List.for_all files ~f:(is_of x) in
    let checked = check all_of_v6 || check all_of_v7 in
    require checked "Ida files must stick to either of 6 or 7 version"

  let run {path; ida;} =
    let exists = Sys.file_exists in
    let exists_sub sub = exists (path / string_of_kind sub) in
    let is_dir = Sys.is_directory in
    let msg_of_kind kind =
      sprintf "%s must exist" (string_of_kind kind) in
    Result.all_ignore [
        require (exists path) "path must exist";
        require (is_dir path) "path must be a folder";
        require (exists_sub ida.graphical)   @@ msg_of_kind ida.graphical;
        require (exists_sub ida.headless)    @@ msg_of_kind ida.headless;
        require (exists_sub ida.graphical64) @@ msg_of_kind ida.graphical64;
        require (exists_sub ida.headless64)  @@ msg_of_kind ida.headless64;
        require (exists (path / "plugins" / "plugin_loader_bap.py"))
          "bap-ida-python must be installed" ]
end

let check = Check.run

let ida_exists path kind =
  Sys.file_exists (path / string_of_kind kind)

let create_ida path =
  let get mode is_headless =
    let kinds = match mode with
      | `m32 when is_headless -> [`idal; `idat]
      | `m64 when is_headless -> [`idal64; `idat64]
      | `m32 -> [`idaq; `ida]
      | `m64 -> [`idaq64; `ida64] in
    List.find ~f:(fun k -> ida_exists path k) kinds |>
      function
      | Some k -> Ok k
      | None ->
         let headless = if is_headless then "headless" else "" in
         let mode = match mode with | `m32 -> 32 | `m64 -> 64 in
         Or_error.errorf "Can't detect %s ida for %d mode" headless mode in
  Or_error.(
    get `m32 true  >>= fun headless ->
    get `m32 false >>= fun graphical ->
    get `m64 true  >>= fun headless64 ->
    get `m64 false >>= fun graphical64 ->
    Ok { headless; graphical; headless64; graphical64 })

let create path is_headless =
  let open Check in
  Or_error.(
    create_ida path >>= fun ida ->
    check_integrity ida >>= fun () ->
    let info = {ida; path; is_headless} in
    check_headless is_headless >>= fun () ->
    check info >>= fun () ->
    Ok info)

let ida32 info =
  if info.is_headless then info.ida.headless
  else info.ida.graphical

let ida64 info =
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
         | None -> info.ida.graphical64 in
    let s = Sexp.to_string (sexp_of_ida_kind kind) in
    let ida = Filename.concat info.path s in
    Filename.quote ida

let is_headless t = t.is_headless
let path t = t.path
