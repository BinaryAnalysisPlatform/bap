open Core_kernel.Std
open Or_error

open Bap.Std
open Elf.Types
open Dwarf

module Fn = Dwarf.Fbi.Fn

let print_fns filename =
  let filedata = In_channel.read_all filename in
  match Elf.Parse.of_string filedata with
  | None -> errorf "%s is not an elf file\n" filename
  | Some elf ->
    let endian = match elf.e_data with
      | ELFDATA2LSB -> LittleEndian
      | ELFDATA2MSB -> BigEndian in
    let create name s = Some (name, Buffer.create s.sh_data) in
    let sections = List.filter_map elf.e_sections ~f:(fun s ->
        match s.sh_name with
        | ".debug_info"   -> create Section.Info s
        | ".debug_abbrev" -> create Section.Abbrev s
        | ".debug_str"    -> create Section.Str s
        | _ -> None) in
    Data.create endian sections >>= fun data ->
    Dwarf.Fbi.create data >>| fun dff ->
    Sequence.iteri (Dwarf.Fbi.functions dff) ~f:(fun i (name,fn) ->
        printf "%02d %s %s %s\n" (i+1)
          (Addr.to_string (Fn.pc_lo fn))
          (Option.value_map (Fn.pc_hi fn) ~default:"unknown"
             ~f:(Addr.to_string))
          name)


let run_printer filename =
  match print_fns filename with
  | Ok () -> ()
  | Error err -> eprintf "Finished with error: %s\n" @@
    Error.to_string_hum err

let arg n = try_with (fun () -> Sys.argv.(n))

let () =
  match arg 1 with
  | Ok arg ->
    let arg = String.strip arg in
    if Sys.file_exists arg then run_printer arg
    else eprintf "No such file: '%s'\n" arg
  | Error _ -> eprintf "Usage:\n\
                        \tdff_test <filename>\n"
