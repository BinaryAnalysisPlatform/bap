open Core_kernel.Std
open Or_error

open Bap.Std
open Elf.Types
open Image_backend
open Image_common

let name = "bap-elf"

let perm_of_flag = function
  | PF_X -> Some X
  | PF_W -> Some W
  | PF_R -> Some R
  | PF_EXT _ -> None


let perm_of_flags = function
  | [] -> errorf "empty flag list"
  | x :: xs ->
    let perm = List.fold xs ~init:None ~f:(fun perm flag ->
        Option.merge perm (perm_of_flag flag)
          ~f:(fun p1 p2 -> Or (p1,p2))) in
    match perm with
    | None -> errorf "invalid set of flags"
    | Some perm -> return perm

let section_data  data s : string Or_error.t =
  int_of_int64 s.sh_size >>= fun size ->
  int_of_int64 s.sh_offset >>= fun offset ->
  try
    let dst = String.create size in
    Bigstring.To_string.blit
      ~src:data ~src_pos:offset
      ~dst ~dst_pos:0 ~len:size;
    return dst
  with exn -> of_exn exn

let create_symtab data endian elf  =
  let module Buffer = Dwarf.Data.Buffer in
  let create name s =
    match section_data data s with
    | Error err -> None          (* TODO something *)
    | Ok data ->   Some (name, Buffer.create data) in
  let sections = Seq.filter_map elf.e_sections ~f:(fun s ->
      let name =
        Elf.section_name data elf s in
      match name with
      | Ok ".debug_info"   -> create Dwarf.Section.Info s
      | Ok ".debug_abbrev" -> create Dwarf.Section.Abbrev s
      | Ok ".debug_str"    -> create Dwarf.Section.Str s
      | Ok _ | Error _ -> None) in
  let sections = Seq.to_list_rev sections in
  Dwarf.Data.create endian sections >>= fun data ->
  Dwarf.Fbi.create data >>= fun dff ->
  let seq = Sequence.mapi (Dwarf.Fbi.functions dff) ~f:(fun i (name,fn) ->
      let pc_lo = Dwarf.Fn.pc_lo fn in
      let size = match Dwarf.Fn.pc_hi fn with
        | None -> return None
        | Some pc_hi ->
          Addr.Int.(!$pc_hi - !$pc_lo) >>= Addr.to_int >>| fun size ->
          Some size in
      size >>= fun size ->
      let location = Location.({
          addr = pc_lo;
          len = Option.value size ~default:1
        }) in
      return {
        Sym.name = name;
        Sym.is_function = true;
        Sym.is_debug = true;
        Sym.locations = location, [];
      }) in
  all (Sequence.to_list_rev seq)

(** @return
    [None] - if section should be skipped as non interesting,
    [Some error] - if an error has occured when we have tried
                   to load section,
    [Some (Ok section)] - if we have loaded section at the end.
*)
let create_section make_addr i es : Section.t Or_error.t option =
  if es.p_type <> PT_LOAD then None
  else
    let section =
      let name = sprintf "%02d" i in
      int_of_int64 es.p_filesz >>= fun len ->
      int_of_int64 es.p_offset >>= fun off ->
      int_of_int64 es.p_memsz  >>= fun vsize ->
      perm_of_flags es.p_flags >>= fun perm ->
      let addr = make_addr es.p_vaddr in
      let location = Location.Fields.create ~len ~addr in
      return  {
        Section.name;
        Section.perm;
        Section.off;
        Section.location;
        Section.vsize;
      } in
    match section with
    | Error _ as err ->
      Some (tag_arg err "skipped segment" i sexp_of_int)
    | ok -> Some ok

let addr_maker = function
  | Word_size.W32 -> fun x -> Addr.of_int32 (Int64.to_int32_exn x)
  | Word_size.W64 -> Addr.of_int64

let img_of_elf data elf : Img.t Or_error.t =
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
  let sections,errors =
    Seq.filter_mapi elf.e_segments (create_section addr) |>
    Seq.to_list |>
    List.partition_map ~f:(function
        | Ok s    -> `Fst s
        | Error e -> `Snd e) in
  let symbols,errors =
    match create_symtab data endian elf with
    | Ok syms -> syms,errors
    | Error err ->
      [], Error.tag err "failed to read symbols" :: errors in
  arch >>= fun arch ->
  match sections with
  | [] -> errorf "failed to read sections"
  | s::ss ->
    let img = Img.Fields.create
        ~arch ~addr_size ~endian ~entry ~sections:(s,ss) ~symbols in
    Ok img

let of_data_err (data : bigstring) : Img.t Or_error.t =
  Elf.Parse.from_bigstring data >>= img_of_elf data

let of_data (data : Bigstring.t) : Img.t option =
  match of_data_err data  with
  | Ok img -> Some img
  | Error err ->
    eprintf "Elf_backend: failed with exn: %s" @@
    Error.to_string_hum err;
    None


let () =
  let r =
    Bap_image.register_backend ~name of_data in
  match r with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "Elf_backend: name «%s» is already used\n" name;
