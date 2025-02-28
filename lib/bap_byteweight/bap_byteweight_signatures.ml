open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
module Sys = Stdlib.Sys

module Config = Bap_main.Extension.Configuration

let (/) = Filename.concat

type error = [
  | `Corrupted of string
  | `No_signatures
  | `No_entry of string
  | `Sys_error of string
]


type 'a data = {
  name : string;
  load : (bytes -> 'a);
  save : ('a -> bytes);
}

exception Failed of error

let fail error = raise (Failed error)
let corrupted entry err = `Corrupted (sprintf "%s: %s" entry err)
let zip_error entry err = fail (corrupted entry err)

let compiler_name =
  Option.value_map ~default:"default" ~f: Theory.Compiler.name

let matches_modulo_bits t name =
  match Theory.Target.matching t name with
  | None -> false
  | Some t' -> Theory.Target.bits t = Theory.Target.bits t'

let matching_entry ?compiler target data {Zip.filename} =
  match String.split filename ~on:'/' with
  | [p1; p2; p3] ->
    matches_modulo_bits target p1 &&
    String.equal (compiler_name compiler) p2 &&
    String.equal data.name p3
  | _ -> fail (`Corrupted ("invalid entry name: " ^ filename))

let with_input file k =
  let zip = Zip.open_in file in
  protect ~finally:(fun () -> Zip.close_in zip) ~f:(fun () -> k zip)

let with_output file k =
  let zip = Zip.open_out file in
  protect ~finally:(fun () -> Zip.close_out zip) ~f:(fun () -> k zip)

let read_entry ?compiler target data file =
  with_input file @@ fun zip ->
  Zip.entries zip |>
  List.find ~f:(matching_entry ?compiler target data) |> function
  | None -> None
  | Some entry ->
    Some (data.load (Bytes.of_string (Zip.read_entry zip entry)))

let read_entries file =
  if Fn.non Sys.file_exists file then []
  else with_input file @@ fun zip ->
    Zip.entries zip |>
    List.map ~f:(fun entry ->
        entry,Zip.read_entry zip entry)

let target_name = Fn.compose KB.Name.unqualified Theory.Target.name

let make_entry ?compiler target data =
  target_name target / compiler_name compiler / data.name

let make_path root = root / "signatures" / "byteweight.zip"

let system_path = make_path Config.sysdatadir

let default_path = match Sys.getenv_opt "BAP_SIGFILE" with
  | Some path -> path
  | None -> make_path Config.datadir

let default_paths = [default_path; system_path]

let try_lookup ?(paths=[]) ?compiler target data =
  paths @ default_paths |> List.find_map ~f:(fun path ->
      if Sys.file_exists path
      then read_entry ?compiler target data path
      else None)

let of_exn = function
  | Sys_error msg -> Error (`Sys_error msg)
  | Zip.Error (_,ent,err) -> Error (corrupted ent err)
  | Failed er -> Error er
  | other -> raise other

let lookup ?paths ?compiler target data =
  match try_lookup ?paths ?compiler target data with
  | exception exn -> of_exn exn
  | None -> Error (`No_entry (target_name target))
  | Some data -> Ok data


let update_or_fail ?compiler target data payload path =
  let entries =
    read_entries path |>
    List.filter ~f:(fun (entry,_) ->
        not (matching_entry ?compiler target data entry)) in
  with_output path @@ fun zip ->
  let path = make_entry ?compiler target data in
  let data = Bytes.unsafe_to_string (data.save payload) in
  Zip.add_entry data zip path;
  List.iter entries ~f:(fun ({Zip.filename; comment; mtime; _},data) ->
      Zip.add_entry data zip filename ~comment ~mtime)

let copy input output =
  let len = 0x1000 in
  let buf = Bytes.create len in
  let rec loop () =
    let read = In_channel.input input ~buf ~pos:0 ~len in
    Out_channel.output output ~buf ~pos:0 ~len:read;
    if read = len then loop () in
  loop ()

let temporary_copy file =
  let tmp,output = Stdlib.Filename.open_temp_file "byteweight" "copy" in
  In_channel.with_file file ~f:(fun input -> copy input output);
  Out_channel.close output;
  tmp

let update ?compiler target data payload path =
  let tmp = temporary_copy path in
  try
    update_or_fail ?compiler target data payload path;
    Sys.rename tmp path;
    Ok ()
  with exn ->
    Sys.remove tmp;
    of_exn exn

module Data = struct
  let registry = Hash_set.create (module String)

  let declare ~load ~save name =
    if Hash_set.mem registry name
    then failwithf "The byteweight data type named %S is \
                    already registered, please pick another name"
        name ();
    Hash_set.add registry name;
    {load; save; name}
end

(* the old deprecated implementation *)

let resolve_path user =
  let user = Option.value_map user ~f:List.return ~default:[] in
  let paths = user @ default_paths in
  match List.find paths ~f:Sys.file_exists with
  | None -> fail `No_signatures
  | Some path -> path

let entry ?(comp="default") ~mode arch =
  Arch.to_string arch / comp / mode

let load_exn ?comp ?path ~mode arch =
  let path = resolve_path path in
  let zip = try Zip.open_in path with
    | Sys_error msg -> fail (`Sys_error msg)
    | Zip.Error (_,ent,err) -> zip_error ent err in
  let entry_path = entry ?comp ~mode arch in
  let r = try
      let entry = Zip.find_entry zip entry_path in
      Ok (Zip.read_entry zip entry |> Stdlib.Bytes.unsafe_of_string)
    with Stdlib.Not_found -> fail (`No_entry entry_path)
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
        if String.(file <> dst) then Zip.add_entry data zip file);
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
