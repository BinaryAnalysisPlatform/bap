open Elf_types
(* value mapping *)

let get_e_class cls =
  match cls with
  | 1 -> ELFCLASS32
  | 2 -> ELFCLASS64
  | _ -> invalid_arg "get_e_class"

let get_e_data data =
  match data with
  | 1 -> ELFDATA2LSB
  | 2 -> ELFDATA2MSB
  | _ -> invalid_arg "get_e_data"

let endian_of ei_data =
    match ei_data with
    | ELFDATA2LSB -> Bitstring.LittleEndian
    | ELFDATA2MSB -> Bitstring.BigEndian

let get_e_osabi abi =
  match abi with
  | 0 -> ELFOSABI_SYSV
  | 1 -> ELFOSABI_HPUX
  | 2 -> ELFOSABI_NETBSD
  | 3 -> ELFOSABI_LINUX
  | 6 -> ELFOSABI_SOLARIS
  | 7 -> ELFOSABI_AIX
  | 8 -> ELFOSABI_IRIX
  | 9 -> ELFOSABI_FREEBSD
  | 10 -> ELFOSABI_TRU64
  | 11 -> ELFOSABI_MODESTO
  | 12 -> ELFOSABI_OPENBSD
  | 64 -> ELFOSABI_ARM_AEABI
  | 97 -> ELFOSABI_ARM
  | 255 -> ELFOSABI_STANDALONE
  | _ -> ELFOSABI_EXT abi

let get_e_type ty =
  match ty with
  | 0 -> ET_NONE
  | 1 -> ET_REL
  | 2 -> ET_EXEC
  | 3 -> ET_DYN
  | 4 -> ET_CORE
  | _ -> ET_EXT ty

let get_e_machine mach =
  match mach with
  | 0 -> EM_NONE
  | 1 -> EM_M32
  | 2 -> EM_SPARC
  | 3 -> EM_386
  | 4 -> EM_68K
  | 5 -> EM_88K
  | 7 -> EM_860
  | 8 -> EM_MIPS
  | 9 -> EM_S370
  | 10 -> EM_MIPS_RS3_LE

  | 15 -> EM_PARISC
  | 17 -> EM_VPP500
  | 18 -> EM_SPARC32PLUS
  | 19 -> EM_960
  | 20 -> EM_PPC
  | 21 -> EM_PPC64
  | 22 -> EM_S390

  | 36 -> EM_V800
  | 37 -> EM_FR20
  | 38 -> EM_RH32
  | 39 -> EM_RCE
  | 40 -> EM_ARM
  | 41 -> EM_ALPHA
  | 42 -> EM_SH
  | 43 -> EM_SPARCV9
  | 44 -> EM_TRICORE
  | 45 -> EM_ARC
  | 46 -> EM_H8_300
  | 47 -> EM_H8_300H
  | 48 -> EM_H8S
  | 49 -> EM_H8_500
  | 50 -> EM_IA_64
  | 51 -> EM_MIPS_X
  | 52 -> EM_COLDFIRE
  | 53 -> EM_68HC12
  | 54 -> EM_MMA
  | 55 -> EM_PCP
  | 56 -> EM_NCPU
  | 57 -> EM_NDR1
  | 58 -> EM_STARCORE
  | 59 -> EM_ME16
  | 60 -> EM_ST100
  | 61 -> EM_TINYJ
  | 62 -> EM_X86_64
  | 63 -> EM_PDSP

  | 66 -> EM_FX66
  | 67 -> EM_ST9PLUS
  | 68 -> EM_ST7
  | 69 -> EM_68HC16
  | 70 -> EM_68HC11
  | 71 -> EM_68HC08
  | 72 -> EM_68HC05
  | 73 -> EM_SVX
  | 74 -> EM_ST19
  | 75 -> EM_VAX
  | 76 -> EM_CRIS
  | 77 -> EM_JAVELIN
  | 78 -> EM_FIREPATH
  | 79 -> EM_ZSP
  | 80 -> EM_MMIX
  | 81 -> EM_HUANY
  | 82 -> EM_PRISM
  | 83 -> EM_AVR
  | 84 -> EM_FR30
  | 85 -> EM_D10V
  | 86 -> EM_D30V
  | 87 -> EM_V850
  | 88 -> EM_M32R
  | 89 -> EM_MN10300
  | 90 -> EM_MN10200
  | 91 -> EM_PJ
  | 92 -> EM_OPENRISC
  | 93 -> EM_ARC_A5
  | 94 -> EM_XTENSA
  | 183 -> EM_AARCH64
  | 188 -> EM_TILEPRO
  | 189 -> EM_MICROBLAZE
  | 191 -> EM_TILEGX
  | _ -> EM_EXT mach

let get_p_type ty =
  match Int32.to_int ty with
  | 0 -> PT_NULL
  | 1 -> PT_LOAD
  | 2 -> PT_DYNAMIC
  | 3 -> PT_INTERP
  | 4 -> PT_NOTE
  | 5 -> PT_SHLIB
  | 6 -> PT_PHDR
  | _ -> PT_OTHER ty

let get_p_flag n =
  match n with
  | 0 -> PF_X
  | 1 -> PF_W
  | 2 -> PF_R
  | _ -> PF_EXT n

let get_sh_type ty =
  match Int32.to_int ty with
  | 0 -> SHT_NULL
  | 1 -> SHT_PROGBITS
  | 2 -> SHT_SYMTAB
  | 3 -> SHT_STRTAB
  | 4 -> SHT_RELA
  | 5 -> SHT_HASH
  | 6 -> SHT_DYNAMIC
  | 7 -> SHT_NOTE
  | 8 -> SHT_NOBITS
  | 9 -> SHT_REL
  | 10 -> SHT_SHLIB
  | 11 -> SHT_DYNSYM
  | _ -> SHT_EXT ty

let get_sh_flag n =
  match n with
  | 0 -> SHF_WRITE
  | 1 -> SHF_ALLOC
  | 2 -> SHF_EXECINSTR
  | _ -> SHF_EXT n

let get_flags flag_of bit data =
  let rec test fs n data =
    if n = bit then
      fs
    else
      let fs =
        if (Int64.logand data Int64.one) = Int64.one then
          (flag_of n) :: fs
        else
          fs
      in
      test fs (n + 1) (Int64.shift_right_logical data 1)
  in
  test [] 0 data

(* elf ident *)
let get_elf_ident ident =
  bitmatch ident with
  | { 0x7f      : 8;
      "ELF"     : 24 : string;
      e_class   : 8;
      e_data    : 8;
      e_version : 8;
      e_osabi   : 8;
      e_abiver  : 8;
      e_pad     : 56;
      rest      : -1 : bitstring
    } ->
      (get_e_class e_class,
       get_e_data e_data,
       e_version,
       get_e_osabi e_osabi,
       e_abiver,
       rest
      )

(* elf header *)
let get_elf_hdr elf =
  let (ei_class, ei_data, ei_version, ei_osabi, ei_abiver, rest) = get_elf_ident elf in
  let endian = endian_of ei_data in
  match ei_class with
  | ELFCLASS32 ->
    (bitmatch rest with
    | { e_type      : 16 : endian (endian);
        e_machine   : 16 : endian (endian);
        e_version   : 32 : endian (endian);
        e_entry     : 32 : endian (endian);
        e_phoff     : 32 : endian (endian);
        e_shoff     : 32 : endian (endian);
        e_flags     : 32 : endian (endian);
        e_ehsize    : 16 : endian (endian);
        e_phentsize : 16 : endian (endian);
        e_phnum     : 16 : endian (endian);
        e_shentsize : 16 : endian (endian);
        e_shnum     : 16 : endian (endian);
        e_shstrndx  : 16 : endian (endian)
      } ->
      ( { e_class = ei_class;
          e_data = ei_data;
          e_version = ei_version;
          e_osabi = ei_osabi;
          e_abiver = ei_abiver;
          e_type = get_e_type e_type;
          e_machine = get_e_machine e_machine;
          e_entry = Int64.of_int32 e_entry;
          e_sections = [];
          e_segments = [];
        },
        { table_offset = Int64.of_int32 e_phoff;
          entry_size = e_phentsize;
          entry_num = e_phnum;
        },
        { table_offset = Int64.of_int32 e_shoff;
          entry_size = e_shentsize;
          entry_num = e_shnum;
        },
        e_shstrndx
      )
    )
  | ELFCLASS64 ->
    bitmatch rest with
    | { e_type      : 16 : endian (endian);
        e_machine   : 16 : endian (endian);
        e_version   : 32 : endian (endian);
        e_entry     : 64 : endian (endian);
        e_phoff     : 64 : endian (endian);
        e_shoff     : 64 : endian (endian);
        e_flags     : 32 : endian (endian);
        e_ehsize    : 16 : endian (endian);
        e_phentsize : 16 : endian (endian);
        e_phnum     : 16 : endian (endian);
        e_shentsize : 16 : endian (endian);
        e_shnum     : 16 : endian (endian);
        e_shstrndx  : 16 : endian (endian)
      } ->
      ( { e_class = ei_class;
          e_data = ei_data;
          e_version = ei_version;
          e_osabi = ei_osabi;
          e_abiver = ei_abiver;
          e_type = get_e_type e_type;
          e_machine = get_e_machine e_machine;
          e_entry = e_entry;
          e_sections = [];
          e_segments = [];
        },
        { table_offset = e_phoff;
          entry_size = e_phentsize;
          entry_num = e_phnum;
        },
        { table_offset = e_shoff;
          entry_size = e_shentsize;
          entry_num = e_shnum;
        },
        e_shstrndx
      )

(* segment *)
let get_segment ei_class endian elf_bit bit =
  match ei_class with
  | ELFCLASS32 ->
    (bitmatch bit with
    | { p_type   : 32 : endian (endian);
        p_offset : 32 : endian (endian);
        p_vaddr  : 32 : endian (endian);
        p_paddr  : 32 : endian (endian);
        p_filesz : 32 : endian (endian);
        p_memsz  : 32 : endian (endian);
        p_flags  : 32 : endian (endian);
        p_align  : 32 : endian (endian)
      } ->
      { p_type = get_p_type p_type;
        p_flags = get_flags get_p_flag 32 (Int64.of_int32 p_flags);
        p_vaddr = Int64.of_int32 p_vaddr;
        p_paddr = Int64.of_int32 p_paddr;
        p_align = Int64.of_int32 p_align;
        p_memsz = Int64.of_int32 p_memsz;
        p_data =
          bitmatch elf_bit with
          | { data : (Int32.to_int p_filesz * 8) : string, offset (Int32.to_int p_offset * 8) } -> data
          | { _ } -> ""
      }
    )
  | ELFCLASS64 ->
    bitmatch bit with
    | { p_type   : 32 : endian (endian);
        p_flags  : 32 : endian (endian);
        p_offset : 64 : endian (endian);
        p_vaddr  : 64 : endian (endian);
        p_paddr  : 64 : endian (endian);
        p_filesz : 64 : endian (endian);
        p_memsz  : 64 : endian (endian);
        p_align  : 64 : endian (endian)
      } ->
      { p_type = get_p_type p_type;
        p_flags = get_flags get_p_flag 32 (Int64.of_int32 p_flags);
        p_vaddr = p_vaddr;
        p_paddr = p_paddr;
        p_align = p_align;
        p_memsz = p_memsz;
        p_data =
          bitmatch elf_bit with
          | { data : (Int64.to_int p_filesz * 8) : string, offset (Int64.to_int p_offset * 8) } -> data
          | { _ } -> ""
      }

(* section *)
let get_section ei_class endian elf_bit sh_str bit =
  let get_c_str ix =
    let buf = Buffer.create 10 in
    let rec loop n =
      if sh_str.[n] = '\x00' then
        Buffer.contents buf
      else
        let () = Buffer.add_char buf sh_str.[n] in
        loop (n + 1)
    in
    loop ix
  in
  match ei_class with
  | ELFCLASS32 ->
    (bitmatch bit with
    | { sh_name      : 32 : endian (endian);
        sh_type      : 32 : endian (endian);
        sh_flags     : 32 : endian (endian);
        sh_addr      : 32 : endian (endian);
        sh_offset    : 32 : endian (endian);
        sh_size      : 32 : endian (endian);
        sh_link      : 32 : endian (endian);
        sh_info      : 32 : endian (endian);
        sh_addralign : 32 : endian (endian);
        sh_entsize   : 32 : endian (endian)
      } ->
      { sh_name = get_c_str (Int32.to_int sh_name);
        sh_type = get_sh_type sh_type;
        sh_flags = get_flags get_sh_flag 32 (Int64.of_int32 sh_flags);
        sh_addr = Int64.of_int32 sh_addr;
        sh_size = Int64.of_int32 sh_size;
        sh_link = sh_link;
        sh_info = sh_info;
        sh_addralign = Int64.of_int32 sh_addralign;
        sh_entsize = Int64.of_int32 sh_entsize;
        sh_data =
          if get_sh_type sh_type = SHT_NOBITS then
            String.create (Int32.to_int sh_size)
          else
            bitmatch elf_bit with
            | { data : (Int32.to_int sh_size * 8) : string, offset (Int32.to_int sh_offset * 8) } -> data
            | { _ } -> ""
      }
    )
  | ELFCLASS64 ->
    bitmatch bit with
    | { sh_name      : 32 : endian (endian);
        sh_type      : 32 : endian (endian);
        sh_flags     : 64 : endian (endian);
        sh_addr      : 64 : endian (endian);
        sh_offset    : 64 : endian (endian);
        sh_size      : 64 : endian (endian);
        sh_link      : 32 : endian (endian);
        sh_info      : 32 : endian (endian);
        sh_addralign : 64 : endian (endian);
        sh_entsize   : 64 : endian (endian)
      } ->
      { sh_name = get_c_str (Int32.to_int sh_name);
        sh_type = get_sh_type sh_type;
        sh_flags = get_flags get_sh_flag 64 sh_flags;
        sh_addr = sh_addr;
        sh_size = sh_size;
        sh_link = sh_link;
        sh_info = sh_info;
        sh_addralign = sh_addralign;
        sh_entsize = sh_entsize;
        sh_data =
          if get_sh_type sh_type = SHT_NOBITS then
            String.create (Int64.to_int sh_size)
          else
            bitmatch elf_bit with
            | { data : (Int64.to_int sh_size * 8) : string, offset (Int64.to_int sh_offset * 8) } -> data;
            | { _ } -> ""
      }

(* main parser *)
let get_sh_offsize ei_class endian bit =
  match ei_class with
  | ELFCLASS32 ->
    (bitmatch bit with
    | { off  : 32 : endian (endian), offset (16 * 8);
        size : 32 : endian (endian)
      } -> (Int64.of_int32 off, Int64.of_int32 size)
    )
  | ELFCLASS64 ->
    bitmatch bit with
    | { off  : 64 : endian (endian), offset (24 * 8);
        size : 64 : endian (endian)
      } -> (off, size)

let table ti bit =
  let rec divide ts n rest =
    if n < ti.entry_num then
      bitmatch rest with
      | { t : (ti.entry_size * 8) : bitstring;
          rest : -1 : bitstring
        } ->
        divide (t :: ts) (n + 1) rest
    else
      ts
  in
  List.rev (divide [] 0 (Bitstring.dropbits (Int64.to_int ti.table_offset * 8) bit))

let of_string elf =
  try
    let bit = Bitstring.bitstring_of_string elf in
    let (elf, seg_ti, sec_ti, e_shstrndx) = get_elf_hdr bit in
    let endian = endian_of elf.e_data in
    let ph = table seg_ti bit in
    let sh = table sec_ti bit in
    let (shstroff, shstrsize) = get_sh_offsize elf.e_class endian (List.nth sh e_shstrndx) in
    let sh_str =
      bitmatch bit with
      | { data : (Int64.to_int shstrsize * 8) : string, offset (Int64.to_int shstroff * 8) } -> data
    in
    let segments = List.map (get_segment elf.e_class endian bit) ph in
    let sections = List.map (get_section elf.e_class endian bit sh_str) sh in
    Some { elf with e_segments = segments; e_sections = sections }
  with _ -> None
