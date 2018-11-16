open Core_kernel
open Regular.Std
open Bap.Std
include Self()

let (/) = Filename.concat

type error = [
  | `Corrupted of string
  | `No_signatures
  | `No_entry of string
  | `Sys_error of string
]

exception Failed of error

let fail error = raise (Failed error)
let zip_error entry err =
  fail (`Corrupted (sprintf "%s: %s" entry err))

let entry ?(comp="default") ~mode arch =
  Arch.to_string arch / comp / mode

let default_path =
  try Sys.getenv "BAP_SIGFILE"
  with Caml.Not_found -> Config.datadir / "sigs.zip"

let load_exn ?comp ?path ~mode arch =
  let path = Option.value path ~default:default_path in
  if not (Sys.file_exists path) then fail `No_signatures;
  let zip = try Zip.open_in path with
    | Sys_error msg -> fail (`Sys_error msg)
    | Zip.Error (_,ent,err) -> zip_error ent err in
  let entry_path = entry ?comp ~mode arch in
  let r = try
      let entry = Zip.find_entry zip entry_path in
      Ok (Zip.read_entry zip entry |> Bytes.of_string)
    with Caml.Not_found -> fail (`No_entry entry_path)
       | Zip.Error (_,ent,err) -> zip_error ent err in
  Zip.close_in zip;
  r

let load ?comp ?path ~mode arch =
  try load_exn ?comp ?path ~mode arch with
  | Failed err -> Error err

(* for some reason Zip truncates the output file, and doesn't provide
   us an option to append anything to for it. *)
let save_exn ?comp ~mode ~path arch data =
  let data = Bytes.to_string data in
  let old = try
      if Sys.file_exists path then
        let zip = Zip.open_in path in
        let ins =
          Zip.entries zip |> List.map ~f:(fun e ->
              e, Zip.read_entry zip e) in
        Zip.close_in zip; ins
      else []
    with Sys_error msg -> fail (`Sys_error msg)
       | Zip.Error (_,ent,err) -> zip_error ent err in
  let zip = try Zip.open_out path with
      Sys_error msg -> fail (`Sys_error msg) in
  try
    let dst = entry ?comp ~mode arch in
    List.iter old ~f:(fun (entry,data) ->
        let file = Zip.(entry.filename) in
        if file <> dst then Zip.add_entry data zip file);
    Zip.add_entry data zip dst;
    Zip.close_out zip
  with Sys_error msg -> fail (`Sys_error msg)
     | Zip.Error (_,ent,err) -> zip_error ent err


let save ?comp ~mode ~path arch data =
  try Ok (save_exn ?comp ~mode ~path arch data)
  with Failed err -> Error err


let string_of_error = function
  | `Corrupted msg -> sprintf "signature database is corrupted: %s" msg
  | `No_signatures -> "signature database doesn't exist"
  | `Sys_error msg -> sprintf "system error: %s" msg
  | `No_entry msg  -> sprintf "can't access given entry: %s" msg
