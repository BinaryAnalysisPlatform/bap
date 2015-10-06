open Core_kernel.Std
open Bap_types.Std

let (/) = Filename.concat


let entry ?(comp="default") ~mode arch =
  Arch.to_string arch / comp / mode

let default_path =
  try Sys.getenv "BAP_SIGFILE"
  with Not_found ->
    Config.prefix / "share" / "bap" / "sigs.zip"

let load_exn ?comp ?path ~mode arch =
  let path = Option.value path ~default:default_path in
  let zip = Zip.open_in path in
  let entry_path = entry ?comp ~mode arch in
  let r = try
      let entry = Zip.find_entry zip entry_path in
      Some (Zip.read_entry zip entry)
    with Not_found -> None in
  Zip.close_in zip;
  r

let load ?comp ?path ~mode arch =
  try load_exn ?comp ?path ~mode arch with exn -> None


(* for some reason Zip truncates the output file, and doesn't provide
   us an option to append anything to for it. *)
let save ?comp ~mode ~path arch data =
  let old =
    if Sys.file_exists path then
      let zip = Zip.open_in path in
      let ins =
        Zip.entries zip |> List.map ~f:(fun e ->
            e, Zip.read_entry zip e) in
      Zip.close_in zip; ins
    else [] in
  let zip = Zip.open_out path in
  let dst = entry ?comp ~mode arch in
  List.iter old ~f:(fun (entry,data) ->
      let file = Zip.(entry.filename) in
      if file <> dst then Zip.add_entry data zip file);
  Zip.add_entry data zip dst;
  Zip.close_out zip
