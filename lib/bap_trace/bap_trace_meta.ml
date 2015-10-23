open Core_kernel.Std
open Bap_types.Std

open Bap_trace_meta_types

module Tracer = struct
  include Tracer
  let pp ppf t =
    Format.fprintf ppf "%s %s" t.name t.version
end

module Binary = struct
  include Binary
  let pp ppf t =
    Format.fprintf ppf "%s: executable%s"
      t.path
      (match t.stripped with
       | Some true -> " stripped"
       | Some false -> " not stripped"
       | None -> "")
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
    let ppt name fmt ft = Format.fprintf fmt "last %s time %a" name pp_time ft in
    Format.fprintf ppf "@[filesize: %d bytes;@ %a;@ %a;@ %a;@]@ " 
      t.size 
      (ppt "access") t.atime
      (ppt "modification") t.mtime 
      (ppt "status change") t.ctime
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
    ~name:"file_stats"
    ~uuid:"f7cde0f9-633d-4a9a-8e71-28fef521bee5"

let trace_ctime =
  Value.Tag.register (module Float)
    ~name:"trace_ctime"
    ~uuid:"b5dd7dbb-7902-4d7d-a236-d5d94a6df885"

let trace_mtime =
  Value.Tag.register (module Float)
    ~name:"trace_mtime"
    ~uuid:"a5576b8f-f9e3-430b-b6bc-738950ea048f"

let user =
  Value.Tag.register (module String)
    ~name:"user"
    ~uuid:"2908b4ea-556f-4cc4-8de1-1b7d9928eadc"

let host =
  Value.Tag.register (module String)
    ~name:"host"
    ~uuid:"4d382d07-b9ea-4f95-a2e1-a3798b0821f0"
