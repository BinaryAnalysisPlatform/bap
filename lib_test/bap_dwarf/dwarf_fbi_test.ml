open Core_kernel.Std
open Or_error

open Bap_types.Std
open Common
open Dwarf

module Buffer = Dwarf_data.Buffer

let print_fns filename =
  let filedata = In_channel.read_all filename in
  match Elf.parse filedata with
  | None -> errorf "%s is not an elf file\n" filename
  | Some elf ->
    let open Elf in
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
    Dwarf_data.create endian sections >>= fun data ->
    Dff.create data >>| fun dff ->
    Sequence.iteri (Dff.functions dff) ~f:(fun i (name,fn) ->
        printf "%02d: %-32s %s\n" (i+1) name @@
        Sexp.to_string_hum (Dff.sexp_of_fn fn))


let run_printer filename =
  match print_fns filename with
  | Ok () -> ()
  | Error err -> eprintf "Finished with error: %s\n" @@
    Error.to_string_hum err


let arg n = try_with (fun () -> Sys.argv.(n))

let () =
  match arg 1 with
  | Ok "inline-test-runner" -> Pa_ounit_lib.Runtime.summarize ()
  | Ok arg ->
    let arg = String.strip arg in
    if Sys.file_exists arg then run_printer arg
    else eprintf "No such file: '%s'\n" arg
  | Error _ -> eprintf "Usage:\n\
                        \tdff_test inline-test-runner dff -show-counts\n\
                        \tdff_test <filename>\n"
