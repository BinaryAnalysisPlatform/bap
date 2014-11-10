open Core_kernel.Std
open Or_error

open Bap.Std
open Elf.Types
open Dwarf

module Fn = Dwarf.Fbi.Fn

let print_fns filename =
  let data =
    Bigstring.of_string (In_channel.read_all filename) in
  Elf.Parse.from_bigstring data >>= fun elf ->
  let endian = match elf.e_data with
    | ELFDATA2LSB -> LittleEndian
    | ELFDATA2MSB -> BigEndian in
  let create name s =
    Elf.string_of_section data s >>= fun section ->
    return (name, Buffer.create section) in
  let sections,errors =
    Seq.to_list elf.e_sections |>
    List.filter_map  ~f:(fun s ->
        match Elf.section_name data elf s with
        | Ok ".debug_info"   -> Some (create Section.Info s)
        | Ok ".debug_abbrev" -> Some (create Section.Abbrev s)
        | Ok ".debug_str"    -> Some (create Section.Str s)
        | Ok _               -> None
        | Error _ as err     -> Some err) |>
    List.partition_map ~f:(function
        | Ok s -> `Fst s
        | Error err -> `Snd err) in
  let () = match errors with
    | [] -> ()
    | errs -> printf "Parsed elf file with warnings:\n";
      List.iter errs ~f:(fun err ->
          printf "%s\n" @@ Error.to_string_hum err) in
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
