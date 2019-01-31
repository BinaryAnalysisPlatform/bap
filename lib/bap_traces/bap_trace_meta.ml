open Core_kernel
open Regular.Std
open Bap.Std
open Format

open Bap_trace_meta_types

module Envp = struct
  let pp ppf env =
    let pp_sep = pp_force_newline in
    pp_print_list ~pp_sep pp_print_string ppf (Array.to_list env)
end

module Args = struct
  let pp ppf args =
    let pp_sep = pp_print_space in
    pp_print_list ~pp_sep pp_print_string ppf (Array.to_list args)
end

module Tracer = struct
  include Tracer
  let pp ppf t =
    fprintf ppf
      "@[<v2>tracer {\
       @\nname:    %s\
       @\nversion: %s\
       @\n@[<2>\
       args:    %a@]\
       @\n@[<2>\
       envp:\
       @\n%a@]\
       @]@\n}" t.name t.version Args.pp t.args Envp.pp t.envp
end

let byte2hexchar = "0123456789abcdef"

let hexstring_of_bytestring str =
  String.init (String.length str * 2) (fun dstix ->
      let srcix = dstix / 2 in
      let is_hi = dstix mod 2 = 0 in
      let b = Caml.Char.code (String.get str srcix) in
      let nib = 0xF land if is_hi then (b lsr 4) else b in
      String.get byte2hexchar nib)

module Binary = struct
  include Binary
  let pp ppf t =
    fprintf ppf
      "@[<v2>binary {\
       @\npath:   %s\
       @\nmd5sum: %s\
       @\n@[<2>\
       args:   %a@]\
       @\n@[<2>envp:@\n%a@]@]@\n}" t.path (Caml.Digest.to_hex t.md5sum) Args.pp t.args Envp.pp t.envp
end

let pp_time fmt t =
  let open Unix in
  let tm = gmtime t in
  Format.fprintf fmt "%02d:%02d:%02d %02d.%02d.%04d GMT"
    tm.tm_hour tm.tm_min tm.tm_sec tm.tm_mday
    tm.tm_mon (1900 + tm.tm_year)

module File_stats = struct
  include File_stats

  let pp ppf t =
    Format.fprintf ppf
      "@[<v2>trace stats {@\nsize: %d@\natime: %a@\nmtime: %a@\nctime: %a@]@\n}"
      t.size pp_time t.atime pp_time t.mtime pp_time t.ctime
end

module Trace_stats = struct
  include Trace_stats
  let pp ppf t =
    fprintf ppf
      "@[Trace created on %s by %s at %a@]" t.host t.user pp_time t.time
end

let tracer =
  Value.Tag.register (module Tracer)
    ~name:"tracer"
    ~uuid:"6e36c74e-580c-4496-9bf6-32cc0a09ef04"

let binary =
  Value.Tag.register (module Binary)
    ~name:"binary"
    ~uuid:"391bb6c3-2c93-4592-b292-43e8ec52e786"

let arch =
  Value.Tag.register (module Arch)
    ~name:"arch"
    ~uuid:"4bffe6d7-c554-45ec-9fb3-102f18e8f390"

let binary_file_stats =
  Value.Tag.register (module File_stats)
    ~name:"file-stats"
    ~uuid:"f7cde0f9-633d-4a9a-8e71-28fef521bee5"

let trace_stats =
  Value.Tag.register (module Trace_stats)
    ~name:"trace-stats"
    ~uuid:"c33eea1b-bca3-4cfe-91b5-52949e8f3c6b"
