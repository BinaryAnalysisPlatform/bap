open Core_kernel.Std
open Regular.Std
open Bap_future.Std
open Bap.Std
open Bap_ida.Std

open Format
open Result.Monad_infix

include Self()

module Symbols = Data.Make(struct
    type t = (string * int64 * int64) list
    let version = "0.1"
  end)

module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  module Factory : sig
    val register : string -> t source -> unit
  end
end


let extract path arch =
  let id =
    Data.Cache.digest ~namespace:"ida" "%s" (Digest.file path) in
  let syms = match Symbols.Cache.load id with
    | Some syms -> syms
    | None -> match Ida.(with_file path get_symbols) with
      | [] ->
        warning "didn't find any symbols";
        info "this plugin doesn't work with IDA Free";
        []
      | syms -> Symbols.Cache.save id syms; syms in
  let size = Arch.addr_size arch in
  let width = Size.in_bits size in
  let addr = Addr.of_int64 ~width in
  List.map syms ~f:(fun (n,s,e) -> n, addr s, addr e) |>
  Seq.of_list


let register_source (module T : Target) =
  let source =
    let open Project.Info in
    let extract file arch = Or_error.try_with (fun () ->
        extract file arch |> T.of_blocks) in
    Stream.merge file arch ~f:extract in
  T.Factory.register name source

let loader_script =
  {|
from bap.utils.ida import dump_loader_info
dump_loader_info('$output')
idc.Exit(0)
|}

type perm = [`code | `data] [@@deriving sexp]
type section = string * perm * int * (int64 * int)
  [@@deriving sexp]

type image = string * addr_size * section list [@@deriving sexp]

module Img = Data.Make(struct
    type t = image
    let version = "0.1"
  end)


exception Unsupported_architecture of string

let arch_of_procname size name = match String.lowercase name with
  | "8086" | "80286r" | "80286p"
  | "80386r" | "80386p"
  | "80486r" | "80486p"
  | "80586r" | "80586p"
  | "80686p" | "k62" | "p2" | "p3" | "athlon" | "p4" | "metapc" ->
    if size = `r32 then `x86 else `x86_64
  | "ppc" ->  if size = `r64 then `ppc64 else `ppc
  | "ppcl" ->  `ppc64
  | "arm" ->  `armv7
  | "armb" ->  `armv7eb
  | "mipsl" ->  if size = `r64 then `mips64el else `mipsel
  | "mipsb" ->  if size = `r64 then `mips64  else `mips
  | "sparcb" -> if size = `r64 then `sparcv9 else `sparc
  | s -> raise (Unsupported_architecture s)

let read_image name =
  In_channel.with_file name ~f:(fun ch ->
      Sexp.input_sexp ch |> image_of_sexp)

let load_image = Command.create `python
    ~script:loader_script
    ~process:read_image


let mapfile path : Bigstring.t =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  let size = Unix.((fstat fd).st_size) in
  let data = Bigstring.map_file ~shared:false fd size in
  Unix.close fd;
  data

let loader path =
  let id = Data.Cache.digest ~namespace:"ida-loader" "%s"
      (Digest.file path) in
  let (proc,size,sections) = match Img.Cache.load id with
    | Some img -> img
    | None ->
      let img = Ida.with_file path load_image in
      Img.Cache.save id img;
      img in
  let bits = mapfile path in
  let arch = arch_of_procname size proc in
  let endian = Arch.endian arch in
  let addr = Addr.of_int64 ~width:(Size.in_bits size) in
  let code,data = List.fold sections
      ~init:(Memmap.empty,Memmap.empty)
      ~f:(fun (code,data) (name,perm,pos,(beg,len)) ->
          match Memory.create ~pos ~len endian (addr beg) bits with
          | Error err ->
            info "skipping section %s: %a" name Error.pp err;
            code,data
          | Ok mem ->
            let sec = Value.create Image.section name in
            if perm = `code
            then Memmap.add code mem sec, data
            else code, Memmap.add data mem sec) in
  Project.Input.create arch path ~code ~data

let main () =
  register_source (module Rooter);
  register_source (module Symbolizer);
  register_source (module Reconstructor);
  Project.Input.register_loader name loader

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "This plugin provides rooter, symbolizer and reconstuctor services.";
      `P "If IDA instance is found on the machine, or specified by a
        user, it will be queried for the specified information."
    ] in
  let path = Config.(param (some string) "path" ~default:None
                       ~doc:"Use IDA to extract symbols from file. \
                             You can optionally provide path to IDA \
                             executable, or executable name.") in
  Config.when_ready (fun {Config.get=(!)} ->
      (* TODO: Use [path] when calling IDA. *)
      main () )
