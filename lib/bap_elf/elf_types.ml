open Core_kernel.Std
open Regular.Std

type e_class =
  | ELFCLASS32
  | ELFCLASS64
[@@deriving bin_io, compare, sexp]

type e_data =
  | ELFDATA2LSB
  | ELFDATA2MSB
[@@deriving bin_io, compare, sexp]

type e_osabi =
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_NETBSD
  | ELFOSABI_LINUX
  | ELFOSABI_SOLARIS
  | ELFOSABI_AIX
  | ELFOSABI_IRIX
  | ELFOSABI_FREEBSD
  | ELFOSABI_TRU64
  | ELFOSABI_MODESTO
  | ELFOSABI_OPENBSD
  | ELFOSABI_ARM_AEABI
  | ELFOSABI_ARM
  | ELFOSABI_STANDALONE
  | ELFOSABI_EXT of int
[@@deriving bin_io, compare, sexp]

type e_type =
  | ET_NONE
  | ET_REL
  | ET_EXEC
  | ET_DYN
  | ET_CORE
  | ET_EXT of int
[@@deriving bin_io, compare, sexp]

type e_machine =
  | EM_NONE
  | EM_M32
  | EM_SPARC
  | EM_386
  | EM_68K
  | EM_88K
  | EM_860
  | EM_MIPS
  | EM_S370
  | EM_MIPS_RS3_LE

  | EM_PARISC
  | EM_VPP500
  | EM_SPARC32PLUS
  | EM_960
  | EM_PPC
  | EM_PPC64
  | EM_S390

  | EM_V800
  | EM_FR20
  | EM_RH32
  | EM_RCE
  | EM_ARM
  | EM_ALPHA
  | EM_SH
  | EM_SPARCV9
  | EM_TRICORE
  | EM_ARC
  | EM_H8_300
  | EM_H8_300H
  | EM_H8S
  | EM_H8_500
  | EM_IA_64
  | EM_MIPS_X
  | EM_COLDFIRE
  | EM_68HC12
  | EM_MMA
  | EM_PCP
  | EM_NCPU
  | EM_NDR1
  | EM_STARCORE
  | EM_ME16
  | EM_ST100
  | EM_TINYJ
  | EM_X86_64
  | EM_PDSP

  | EM_FX66
  | EM_ST9PLUS
  | EM_ST7
  | EM_68HC16
  | EM_68HC11
  | EM_68HC08
  | EM_68HC05
  | EM_SVX
  | EM_ST19
  | EM_VAX
  | EM_CRIS
  | EM_JAVELIN
  | EM_FIREPATH
  | EM_ZSP
  | EM_MMIX
  | EM_HUANY
  | EM_PRISM
  | EM_AVR
  | EM_FR30
  | EM_D10V
  | EM_D30V
  | EM_V850
  | EM_M32R
  | EM_MN10300
  | EM_MN10200
  | EM_PJ
  | EM_OPENRISC
  | EM_ARC_A5
  | EM_XTENSA
  | EM_AARCH64
  | EM_TILEPRO
  | EM_MICROBLAZE
  | EM_TILEGX
  | EM_EXT of int
[@@deriving bin_io, compare, sexp]

type p_type =
  | PT_NULL
  | PT_LOAD
  | PT_DYNAMIC
  | PT_INTERP
  | PT_NOTE
  | PT_SHLIB
  | PT_PHDR
  | PT_OTHER of int32
[@@deriving bin_io, compare, sexp]

type p_flag =
  | PF_X
  | PF_W
  | PF_R
  | PF_EXT of int
[@@deriving bin_io, compare, sexp]

type sh_type =
  | SHT_NULL
  | SHT_PROGBITS
  | SHT_SYMTAB
  | SHT_STRTAB
  | SHT_RELA
  | SHT_HASH
  | SHT_DYNAMIC
  | SHT_NOTE
  | SHT_NOBITS
  | SHT_REL
  | SHT_SHLIB
  | SHT_DYNSYM
  | SHT_EXT of int32
[@@deriving bin_io, compare, sexp]

type sh_flag =
  | SHF_WRITE
  | SHF_ALLOC
  | SHF_EXECINSTR
  | SHF_EXT of int
[@@deriving bin_io, compare, sexp]

type segment = {
  p_type   : p_type;
  p_flags  : p_flag list;
  p_vaddr  : int64;
  p_paddr  : int64;
  p_align  : int64;
  p_memsz  : int64;
  p_filesz : int64;
  p_offset : int64;
} [@@deriving bin_io, compare, fields, sexp]

type section = {
  sh_name : int;
  sh_type : sh_type;
  sh_flags : sh_flag list;
  sh_addr : int64;
  sh_size : int64;
  sh_link : int32;
  sh_info : int32;
  sh_addralign : int64;
  sh_entsize : int64;
  sh_offset : int64;
} [@@deriving bin_io, compare, fields, sexp]


type elf = {
  e_class : e_class;
  e_data : e_data;
  e_version : int;
  e_osabi : e_osabi;
  e_abiver : int;
  e_type : e_type;
  e_machine : e_machine;
  e_entry : int64;
  e_shstrndx : int;
  e_sections : section seq;
  e_segments : segment seq;
} [@@deriving bin_io, compare, fields, sexp]

type table_info = {
  table_offset : int64;
  entry_size : int;
  entry_num : int;
} [@@deriving bin_io, compare, fields, sexp]
