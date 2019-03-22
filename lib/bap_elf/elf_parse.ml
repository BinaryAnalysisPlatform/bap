(*
 * Copyright (c) 2014 Carnegie Mellon University
 * Copyright (c) 2013, Jyun-Yan You
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the author nor the names of his contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *)
open Core_kernel

open Bitstring
open Elf_types
open Elf_internal_utils

module Char = Caml.Char

let elf_max_header_size = 64         (* no magick! *)

(** value mapping *)

let parse_e_class cls =
  match cls with
  | 1 -> ELFCLASS32
  | 2 -> ELFCLASS64
  | _ -> invalid_arg "parse_e_class"

let parse_e_data data =
  match data with
  | 1 -> ELFDATA2LSB
  | 2 -> ELFDATA2MSB
  | _ -> invalid_arg "parse_e_data"

let endian_of ei_data =
  match ei_data with
  | ELFDATA2LSB -> LittleEndian
  | ELFDATA2MSB -> BigEndian

let parse_e_osabi abi =
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

let parse_e_type ty =
  match ty with
  | 0 -> ET_NONE
  | 1 -> ET_REL
  | 2 -> ET_EXEC
  | 3 -> ET_DYN
  | 4 -> ET_CORE
  | _ -> ET_EXT ty

let parse_e_machine mach =
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

let parse_p_type = function
  | 0l -> PT_NULL
  | 1l -> PT_LOAD
  | 2l -> PT_DYNAMIC
  | 3l -> PT_INTERP
  | 4l -> PT_NOTE
  | 5l -> PT_SHLIB
  | 6l -> PT_PHDR
  | n -> PT_OTHER n

let parse_p_flag n =
  match n with
  | 0 -> PF_X
  | 1 -> PF_W
  | 2 -> PF_R
  | _ -> PF_EXT n

let parse_sh_type = function
  | 0l -> SHT_NULL
  | 1l -> SHT_PROGBITS
  | 2l -> SHT_SYMTAB
  | 3l -> SHT_STRTAB
  | 4l -> SHT_RELA
  | 5l -> SHT_HASH
  | 6l -> SHT_DYNAMIC
  | 7l -> SHT_NOTE
  | 8l -> SHT_NOBITS
  | 9l -> SHT_REL
  | 10l -> SHT_SHLIB
  | 11l -> SHT_DYNSYM
  | n -> SHT_EXT n

let parse_sh_flag n =
  match n with
  | 0 -> SHF_WRITE
  | 1 -> SHF_ALLOC
  | 2 -> SHF_EXECINSTR
  | _ -> SHF_EXT n

let parse_flags flag_of bit data =
  let rec test fs n data =
    if n = bit then fs
    else
      let fs =
        if Int64.bit_and data 1L = 1L
        then flag_of n :: fs
        else fs in
      test fs (n + 1) (Int64.shift_right_logical data 1) in
  test [] 0 data

(** elf ident *)

let parse_elf_ident bits =
  match%bitstring bits with
  | {|0x7f      : 8;
      "ELF"     : 24 : string;
      e_class   : 8;
      e_data    : 8;
      e_version : 8;
      e_osabi   : 8;
      e_abiver  : 8;
      e_pad     : 56;
      rest      : -1 : bitstring
    |} ->
    (parse_e_class e_class,
     parse_e_data e_data,
     e_version,
     parse_e_osabi e_osabi,
     e_abiver,
     rest)

(* elf header *)
let parse_elf_hdr elf =
  let (ei_class, ei_data, ei_version, ei_osabi, ei_abiver, rest) =
    parse_elf_ident elf in
  let endian = endian_of ei_data in
  match ei_class with
  | ELFCLASS32 ->  (match%bitstring rest with
      | {|e_type      : 16 : endian (endian);
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
        |} ->
        let elf = {
          e_class = ei_class;
          e_data = ei_data;
          e_version = ei_version;
          e_osabi = ei_osabi;
          e_abiver = ei_abiver;
          e_type = parse_e_type e_type;
          e_machine = parse_e_machine e_machine;
          e_entry = Int64.of_int32 e_entry;
          e_sections = Sequence.empty;
          e_segments = Sequence.empty;
          e_shstrndx;
        } in
        let seg_table = {
          table_offset = Int64.of_int32 e_phoff;
          entry_size = e_phentsize;
          entry_num = e_phnum;
        } in
        let sec_table = {
          table_offset = Int64.of_int32 e_shoff;
          entry_size = e_shentsize;
          entry_num = e_shnum;
        } in
        elf, seg_table, sec_table
    )
  | ELFCLASS64 -> ( match%bitstring rest with
      | {|e_type      : 16 : endian (endian);
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
        |} ->
        let elf = {
          e_class = ei_class;
          e_data = ei_data;
          e_version = ei_version;
          e_osabi = ei_osabi;
          e_abiver = ei_abiver;
          e_type = parse_e_type e_type;
          e_machine = parse_e_machine e_machine;
          e_entry = e_entry;
          e_shstrndx;
          e_sections = Sequence.empty;
          e_segments = Sequence.empty;
        } in
        let seg_table = {
          table_offset = e_phoff;
          entry_size = e_phentsize;
          entry_num = e_phnum;
        } in
        let sec_table = {
          table_offset = e_shoff;
          entry_size = e_shentsize;
          entry_num = e_shnum;
        } in
        elf, seg_table, sec_table
    )

(* segment *)
let parse_segment ei_class endian bit =
  match ei_class with
  | ELFCLASS32 -> (match%bitstring bit with
      | {|p_type   : 32 : endian (endian);
          p_offset : 32 : endian (endian);
          p_vaddr  : 32 : endian (endian);
          p_paddr  : 32 : endian (endian);
          p_filesz : 32 : endian (endian);
          p_memsz  : 32 : endian (endian);
          p_flags  : 32 : endian (endian);
          p_align  : 32 : endian (endian)
        |} -> {
          p_type   = parse_p_type p_type;
          p_flags  = parse_flags parse_p_flag 32 (Int64.of_int32 p_flags);
          p_vaddr  = Int64.of_int32 p_vaddr;
          p_paddr  = Int64.of_int32 p_paddr;
          p_align  = Int64.of_int32 p_align;
          p_memsz  = Int64.of_int32 p_memsz;
          p_filesz = Int64.of_int32 p_filesz;
          p_offset = Int64.of_int32 p_offset;
        })
  | ELFCLASS64 -> (match%bitstring bit with
      | {|p_type   : 32 : endian (endian);
          p_flags  : 32 : endian (endian);
          p_offset : 64 : endian (endian);
          p_vaddr  : 64 : endian (endian);
          p_paddr  : 64 : endian (endian);
          p_filesz : 64 : endian (endian);
          p_memsz  : 64 : endian (endian);
          p_align  : 64 : endian (endian)
        |} -> {
          p_type  = parse_p_type p_type;
          p_flags = parse_flags parse_p_flag 32 (Int64.of_int32 p_flags);
          p_vaddr;
          p_paddr;
          p_align;
          p_memsz;
          p_filesz;
          p_offset;
        })

(* section *)
let parse_section ei_class endian bit =
  match ei_class with
  | ELFCLASS32 -> (match%bitstring bit with
      | {|sh_name      : 32 : endian (endian);
          sh_type      : 32 : endian (endian);
          sh_flags     : 32 : endian (endian);
          sh_addr      : 32 : endian (endian);
          sh_offset    : 32 : endian (endian);
          sh_size      : 32 : endian (endian);
          sh_link      : 32 : endian (endian);
          sh_info      : 32 : endian (endian);
          sh_addralign : 32 : endian (endian);
          sh_entsize   : 32 : endian (endian)
        |} -> {
          sh_name = Int32.to_int_exn sh_name;
          sh_type = parse_sh_type sh_type;
          sh_flags = parse_flags parse_sh_flag 32 (Int64.of_int32 sh_flags);
          sh_addr = Int64.of_int32 sh_addr;
          sh_size = Int64.of_int32 sh_size;
          sh_link = sh_link;
          sh_info = sh_info;
          sh_addralign = Int64.of_int32 sh_addralign;
          sh_entsize = Int64.of_int32 sh_entsize;
          sh_offset = Int64.of_int32 sh_offset;
        })
  | ELFCLASS64 -> (match%bitstring bit with
      | {|
        sh_name      : 32 : endian (endian);
        sh_type      : 32 : endian (endian);
        sh_flags     : 64 : endian (endian);
        sh_addr      : 64 : endian (endian);
        sh_offset    : 64 : endian (endian);
        sh_size      : 64 : endian (endian);
        sh_link      : 32 : endian (endian);
        sh_info      : 32 : endian (endian);
        sh_addralign : 64 : endian (endian);
        sh_entsize   : 64 : endian (endian)
      |} -> {
          sh_name = Int32.to_int_exn sh_name;
          sh_type = parse_sh_type sh_type;
          sh_flags = parse_flags parse_sh_flag 64 sh_flags;
          sh_addr = sh_addr;
          sh_link = sh_link;
          sh_info = sh_info;
          sh_addralign = sh_addralign;
          sh_entsize = sh_entsize;
          sh_size;
          sh_offset;
        })

let validate_offsets desc  ~pos ~len ~offset ~size : unit Or_error.t =
  Validate.(result @@ name_list desc [
      name "size" @@ Int.validate_bound size
        ~min:(Incl 0) ~max:(Incl len);
      name "offset" @@ Int.validate_bound offset
        ~min:(Incl pos)
        ~max:(Excl (pos + len));
      name "size+offset" @@
      Int.validate_ubound (size + offset)
        ~max:(Incl (pos + len))
    ])


let split bits size : bitstring * bitstring =
  match%bitstring bits with
  | {|hd : size * 8 : bitstring;
      tl : -1       : bitstring|} -> hd,tl


(* Bitlength needed that's why lsl 3 *)
let bitstring_of_bytes b = b, 0, Bytes.length b lsl 3

(** returns a sequence of table entries  *)
let split_table ti ~pos ~len data : bitstring Sequence.t Or_error.t =
  let open Or_error in
  let table_size = ti.entry_size * ti.entry_num in
  int_of_int64 ti.table_offset >>= fun offset ->
  validate_offsets "table" ~pos ~len ~offset ~size:table_size
  >>= fun () ->
  let bits = Bytes.create table_size in
  Bigstring.To_bytes.blit
    ~src:data ~src_pos:(pos + offset) ~dst:bits ~dst_pos:0 ~len:table_size;
  let t = Sequence.unfold ~init:(bitstring_of_bytes bits)
      ~f:(fun bits -> Some (split bits ti.entry_size)) in
  return (Sequence.take t ti.entry_num)

let validate_bounds ~pos ~len ~data_len =
  Validate.(result @@ name_list "bigstring bounds" [
      name "pos in bounds" @@ Int.validate_bound pos
        ~min:(Incl 0) ~max:(Excl data_len);
      name "len in bounds" @@ Int.validate_bound len
        ~min:(Incl elf_max_header_size) ~max:(Incl data_len);
      name "pos+len in bounds" @@ Int.validate_ubound (pos+len)
        ~max:(Incl data_len)
    ])

let from_bigstring_exn ?(pos=0) ?len data =
  let open Or_error in
  let data_len = Bigstring.length data in
  let len = Option.value len ~default:data_len in
  validate_bounds ~pos ~len ~data_len >>= fun () ->
  let header = Bytes.create elf_max_header_size in
  Bigstring.To_bytes.blit
    ~src:data ~src_pos:pos ~dst:header ~dst_pos:0
    ~len:elf_max_header_size;
  let (elf, seg_ti, sec_ti) =
    parse_elf_hdr (bitstring_of_bytes header) in
  let endian = endian_of elf.e_data in
  split_table seg_ti ~pos ~len data >>= fun ph ->
  split_table sec_ti ~pos ~len data >>= fun sh ->
  let parse_table f = Sequence.map ~f:(f elf.e_class endian) in
  let e_segments = parse_table parse_segment ph in
  let e_sections = parse_table parse_section sh in
  return { elf with e_segments; e_sections }

let from_bigstring ?pos ?len data =
  Or_error.try_with_join ~backtrace:true (fun () -> from_bigstring_exn ?pos ?len data)
