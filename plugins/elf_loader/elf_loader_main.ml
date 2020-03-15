open Core_kernel
open Or_error

open Bap.Std
open Bap_elf.Std
open Bap_dwarf.Std
open Backend
[@@warning "-D"]

open Elf

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "An ELF image loader written in pure OCaml.";
    `S "SEE ALSO";
    `P "$(b,bap-elf)(3), $(b,bap-plugin-llvm)(1)"
  ]

let name = "bap-elf"

let perm_of_flag = function
  | Elf.PF_X -> Some X
  | Elf.PF_W -> Some W
  | Elf.PF_R -> Some R
  | Elf.PF_EXT _ -> None

let int_of_int64 n = match Int64.to_int n with
  | Some v -> Ok v
  | None -> Or_error.errorf "number %LdL doesn't fit into int" n


let perm_of_flags = function
  | [] -> errorf "empty flag list"
  | xs ->
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
    let dst = Bytes.create size in
    Bigstring.To_bytes.blit
      ~src:data ~src_pos:offset
      ~dst ~dst_pos:0 ~len:size;
    return (Bytes.to_string dst)
  with exn -> of_exn exn

let create_symtab data endian elf  =
  let module Buffer = Dwarf.Buffer in
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
          Addr.Int_err.(!$pc_hi - !$pc_lo) >>= Addr.to_int >>| fun size ->
          Some size in
      size >>= fun size ->
      let location = Location.({
          addr = pc_lo;
          len = Option.value size ~default:1
        }) in
      return {
        Symbol.name = name;
        is_function = true;
        is_debug = true;
        locations = location, [];
      }) in
  all (Sequence.to_list_rev seq)

(** @return
    [None] - if segment should be skipped as non interesting,
    [Some error] - if an error has occurred when we have tried
                   to load segment,
    [Some (Ok segment)] - if we have loaded segment at the end.
*)
let create_segment make_addr i es : Segment.t Or_error.t option =
  if not ([%compare.equal : p_type] es.p_type PT_LOAD) then None
  else
    let segment =
      let name = sprintf "%02d" i in
      int_of_int64 es.p_filesz >>= fun len ->
      int_of_int64 es.p_offset >>= fun off ->
      perm_of_flags es.p_flags >>= fun perm ->
      let addr = make_addr es.p_vaddr in
      let location = Location.Fields.create ~len ~addr in
      return  {
        Segment.name;
        Segment.perm;
        Segment.off;
        Segment.location;
      } in
    match segment with
    | Error _ as err ->
      Some (tag_arg err "skipped segment" i sexp_of_int)
    | ok -> Some ok

let addr_maker = function
  | `r32 -> Addr.of_int64 ~width:32
  | `r64 -> Addr.of_int64 ~width:64

let img_of_elf data elf : Img.t Or_error.t =
  let endian = match elf.e_data with
    | ELFDATA2LSB -> LittleEndian
    | ELFDATA2MSB -> BigEndian in
  let addr_size = match elf.e_class with
    | ELFCLASS32 -> `r32
    | ELFCLASS64 -> `r64 in
  let addr = addr_maker addr_size in
  let entry = addr elf.e_entry in
  let arch = match elf.e_machine, endian,addr_size with
    | EM_386, _,_ -> Ok `x86
    | EM_X86_64, _,_ -> Ok `x86_64
    | EM_ARM, LittleEndian,_ -> Ok `armv7
    | EM_ARM, BigEndian,_ -> Ok `armv7eb
    | EM_AARCH64, LittleEndian,_ -> Ok `aarch64
    | EM_AARCH64, BigEndian,_ -> Ok `aarch64_be
    | EM_SPARC,_,_ -> Ok `sparc
    | EM_SPARCV9,_ ,_-> Ok `sparcv9
    | EM_PPC,_,_ -> Ok `ppc
    | EM_PPC64, BigEndian,_ -> Ok `ppc64
    | EM_PPC64, LittleEndian,_  -> Ok `ppc64le
    | EM_S390,_,_ -> Ok `systemz
    | EM_MIPS, BigEndian,`r32 -> Ok `mips
    | EM_MIPS, LittleEndian, `r32 -> Ok `mipsel
    | EM_MIPS, BigEndian,`r64 -> Ok `mips64
    | EM_MIPS, LittleEndian, `r64 -> Ok `mips64el
    | _ -> errorf "can't load file, unsupported platform" in
  let segments,errors =
    Seq.filter_mapi elf.e_segments (create_segment addr) |>
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
  let sections = Seq.filter_map elf.e_sections ~f:(fun s ->
      match Elf.section_name data elf s with
      | Error _ -> None
      | Ok name -> Some {
          Section.name;
          Section.location = {
            Location.addr = addr s.sh_addr;
            Location.len  = Int64.to_int_exn s.sh_size;
          }
        }) |> Seq.to_list in
  match segments with
  | [] -> errorf "failed to read segments"
  | s::ss ->
    return @@
    Img.Fields.create ~arch ~entry ~segments:(s,ss) ~symbols ~sections

let of_data_err (data : bigstring) : Img.t Or_error.t =
  Elf.from_bigstring data >>= img_of_elf data

let of_data (data : Bigstring.t) : Img.t option =
  match of_data_err data with
  | Ok img -> Some img
  | Error err ->
    eprintf "Elf_backend: failed with exn: %s" @@
    Error.to_string_hum err;
    None

let () =
  Config.when_ready @@ fun _ ->
  match Image.register_backend ~name of_data with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "Elf_backend: name «%s» is already used\n" name
[@@warning "-D"]
