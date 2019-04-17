open Core_kernel
open Elf_types
open Elf_internal_utils
open Or_error

let input_string0 ~pos data : string Or_error.t =
  match Bigstring.find ~pos  '\x00' data with
  | None -> errorf "string untermintated"
  | Some last ->
    let len = last - pos in
    let dst = Bytes.create len in
    Bigstring.To_bytes.blit
      ~src:data ~src_pos:pos ~dst ~dst_pos:0 ~len;
    return (Bytes.to_string dst)

let section_name data elf section =
  match Sequence.nth elf.e_sections elf.e_shstrndx with
  | None -> errorf "index %d doesn't point to a valid section" elf.e_shstrndx
  | Some sec ->
    int_of_int64 sec.sh_offset >>= fun s_off ->
    let off = s_off + section.sh_name in
    Validate.(result @@ name "off" @@
              Int.validate_bound off
                ~min:(Incl 0) ~max:(Excl (Bigstring.length data)))
    >>= fun () -> input_string0 ~pos:off data

let string_of_section  data s : string Or_error.t =
  int_of_int64 s.sh_size >>= fun size ->
  int_of_int64 s.sh_offset >>= fun offset ->
  try
    let dst = Bytes.create size in
    Bigstring.To_bytes.blit
      ~src:data ~src_pos:offset
      ~dst ~dst_pos:0 ~len:size;
    return (Bytes.to_string dst)
  with exn -> of_exn exn
