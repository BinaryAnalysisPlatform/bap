open Core_kernel.Std
open Or_error

open Bap_core.Std

open Elf.Types
open Image_backend

let name = "ocaml-elf"

let perm_of_flags flags =
  let init = {x = false; w = false; r = false} in
  List.fold flags ~init ~f:(fun perm -> function
      | PF_X -> { perm with x = true }
      | PF_W -> { perm with w = true }
      | PF_R -> { perm with r = true }
      | _ -> perm)

let create_symtab endian elf =
  let module Buffer = Dwarf.Data.Buffer in
  let create name s = Some (name, Buffer.create s.sh_data) in
  let sections = List.filter_map elf.e_sections ~f:(fun s ->
      match s.sh_name with
      | ".debug_info"   -> create Dwarf.Section.Info s
      | ".debug_abbrev" -> create Dwarf.Section.Abbrev s
      | ".debug_str"    -> create Dwarf.Section.Str s
      | _ -> None) in
  Dwarf.Data.create endian sections >>= fun data ->
  Dwarf.Fbi.create data >>= fun dff ->
  let seq = Sequence.mapi (Dwarf.Fbi.functions dff) ~f:(fun i (name,fn) ->
      let pc_lo = Dwarf.Fn.pc_lo fn in
      let size = match Dwarf.Fn.pc_hi fn with
        | None -> return None
        | Some pc_hi ->
          Addr.Int.(!$pc_hi - !$pc_lo) >>= Addr.to_int >>| fun size ->
          Some size in
      size >>= fun size -> return {
        Sym.name = Some name;
        Sym.kind = `func;
        Sym.addr = pc_lo;
        Sym.size = size
      }) in
  all (Sequence.to_list_rev seq)

let create_section addr i es =
  let name = sprintf "%02d" i in
  let name = if es.p_type = PT_LOAD then Some name else None in
  let open Option.Monad_infix in
  name >>= fun name -> Int64.to_int es.p_memsz >>= function
  | 0 -> None
  | size ->
    let data = Bigstring.init size ~f:(fun (_:int) -> '\000') in
    Bigstring.From_string.blito ~src:es.p_data ~dst:data ();
    Some {
      Section.name;
      Section.addr = addr es.p_vaddr;
      Section.perm = perm_of_flags es.p_flags;
      Section.off = 0;
      Section.size = String.length es.p_data;
      Section.data;
    }


let addr_maker = function
  | Word_size.W32 -> fun x -> Addr.of_int32 (Int64.to_int32_exn x)
  | Word_size.W64 -> Addr.of_int64


let img_of_elf elf : Img.t Or_error.t =
  let endian = match elf.e_data with
    | ELFDATA2LSB -> LittleEndian
    | ELFDATA2MSB -> BigEndian in
  let addr_size = match elf.e_class with
    | ELFCLASS32 -> Word_size.W32
    | ELFCLASS64 -> Word_size.W64 in
  let addr = addr_maker addr_size in
  let entry = addr elf.e_entry in
  let arch = match elf.e_machine with
    | EM_386 -> Ok Arch.X86_32
    | EM_X86_64 -> Ok Arch.X86_64
    | EM_ARM -> Ok Arch.ARM
    | _ -> errorf "can't load file, unsupported platform" in
  let sections =
    Array.of_list @@
    List.filter_mapi elf.e_segments (create_section addr) in
  let symbols = match create_symtab endian elf with
    | Ok syms -> Array.of_list_rev syms
    | Error err ->
      eprintf "Elf_backend: failed to read symbols: %s\n" @@
      Error.to_string_hum err;
      [| |] in
  arch >>= fun arch ->
  if Array.length sections = 0
  then errorf "failed to read sections"
  else Ok (Img.Fields.create
             ~arch ~addr_size ~endian ~entry ~sections ~symbols)

let of_data_exn (data : bigstring) : Img.t option =
  match Elf.Parse.of_string (Bigstring.to_string data) with
  | None -> eprintf "Elf_backend: failed for unknown reason\n"; None
  | Some elf -> match img_of_elf elf with
    | Ok img -> Some img
    | Error err ->
      eprintf "Elf_backend: %s\n" @@
      Error.to_string_hum err;
      None

let of_data data =
  try of_data_exn data
  with exn ->
    eprintf "Elf_backend: failed with exn: %s" @@
    Exn.to_string exn;
    None


let () =
  let r =
    Bap_image.register_backend ~name {of_data; to_data = None;} in
  match r with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "Elf_backend: name «%s» is already used\n" name;
