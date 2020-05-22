open Bap_knowledge
open Core_kernel
open Regular.Std
open Bap_future.Std
open Bap.Std
open Bap_ida.Std

open Format
open Result.Monad_infix

include Self()

module Info = Bap_ida_info

module Symbols = Data.Make(struct
    type t = (string * int64 * int64) list
    let version = "1.0.0"
  end)

module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  val provide : Knowledge.agent -> t -> unit
end

let digest = Caml.Digest.file

let request =
  sprintf "
from bap.utils import ida
ida.service.request(service='%s', output='$output')
"

let get_symbols =
  Command.create
    `python
    ~script:(request "symbols")
    ~parser:(fun name ->
        let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
        In_channel.with_file name ~f:(fun ch ->
            Sexp.input_sexps ch |> List.map ~f:blk_of_sexp))

let extract path arch =
  let id =
    Data.Cache.digest ~namespace:"ida" "%s" (digest path) in
  let syms = match Symbols.Cache.load id with
    | Some syms -> syms
    | None -> match Ida.(with_file path get_symbols) with
      | [] ->
        warning "didn't find any symbols";
        info "note: this plugin doesn't work with IDA Free";
        []
      | syms -> Symbols.Cache.save id syms; syms in
  let size = Arch.addr_size arch in
  let width = Size.in_bits size in
  let addr = Addr.of_int64 ~width in
  List.map syms ~f:(fun (n,s,e) -> n, addr s, addr e) |>
  Seq.of_list

let ida_symbolizer =
  let reliability = Knowledge.Agent.reliable in
  Knowledge.Agent.register ~reliability
    ~package:"bap.std" "ida-symbolizer"
    ~desc:"Provides information from IDA Pro"

let register_source (module T : Target) =
  let inputs = Stream.zip Project.Info.file Project.Info.arch in
  Stream.observe inputs @@ fun (file,arch) ->
  T.provide ida_symbolizer (T.of_blocks (extract file arch))


type perm = [`code | `data] [@@deriving sexp, equal]
type section = string * perm * int * (int64 * int)
[@@deriving sexp]

type image = string * addr_size * section list [@@deriving sexp]

module Img = Data.Make(struct
    type t = image
    let version = "1.0.0"
  end)


exception Unsupported_architecture of string

let arch_of_procname size name =
  let (=) = [%compare.equal: addr_size] in
  match String.lowercase name with
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
    ~script:(request "loader")
    ~parser:read_image


let mapfile path : Bigstring.t =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  let size = Unix.((fstat fd).st_size) in
  let data =
    Mmap.V1.map_file
      fd Bigarray.char Bigarray.c_layout false [|size|] in
  Unix.close fd;
  Bigarray.array1_of_genarray data
[@@warning "-D"]

let loader path =
  let id = Data.Cache.digest ~namespace:"ida-loader" "%s"
      (digest path) in
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
            if equal_perm perm `code
            then Memmap.add code mem sec, data
            else code, Memmap.add data mem sec) in
  Project.Input.create arch path ~code ~data

(* The ida service named brancher provides data as a list of
 * 3-tuples of the form (addr, jump of normal flow (fall) type,
 * jumps of other types) *)
type brancher_info = (int64 * int64 option * int64 list) list
[@@deriving sexp]

module IdaBrancher : sig
  type t
  val of_brancher_info : arch -> brancher_info -> t
  val resolve : t -> mem -> Disasm_expert.Basic.full_insn -> Brancher.dests
  include Data.S with type t := t
end = struct
  type t = Brancher.dests Addr.Table.t

  include Data.Make(struct
      type nonrec t = t
      let version = "0.1"
    end)

  let int64_to_word arch =
    let addr_size = Arch.addr_size arch in
    (Word.of_int64 ~width:(Size.in_bits addr_size))

  let of_brancher_info arch bf : t =
    let (!) = (int64_to_word arch) in
    let other_flows_to_dests flows =
      List.fold flows ~init:[] ~f:(fun acc addr ->
          (Some !addr, `Jump)::acc) in
    let helper tab (addr,_normal_flow,other_flows) =
      Addr.Table.add_exn tab ~key:!addr
        ~data:(other_flows_to_dests other_flows);
      tab
    in
    List.fold bf
      ~init:(Addr.Table.create ())
      ~f:helper

  let resolve t mem _ =
    match
      Addr.Table.find t (Memory.min_addr mem)
    with
    | Some dests -> dests
    | None -> []
end

let read_brancher_info arch name =
  In_channel.with_file name ~f:(fun ch ->
      Sexp.input_sexp ch |> brancher_info_of_sexp
      |> (IdaBrancher.of_brancher_info arch))

let load_brancher_info arch =
  Command.create
    `python
    ~script:(request "brancher")
    ~parser:(read_brancher_info arch)

let get_resolve_fun file arch =
  let id = Data.Cache.digest ~namespace:"ida-brancher" "%s"
      (digest file) in
  let brancher = match IdaBrancher.Cache.load id with
    | Some i -> i
    | None ->
      let i = Ida.with_file file (load_brancher_info arch) in
      IdaBrancher.Cache.save id i;
      i in
  (IdaBrancher.resolve brancher)

let register_brancher_source () =
  let inputs =
    Stream.zip Project.Info.file Project.Info.arch in
  Stream.observe inputs @@ fun (file,arch) ->
  Brancher.provide @@ Brancher.create (get_resolve_fun file arch)

let main () =
  register_source (module struct include Rooter
    let provide _ data = provide data
  end);
  register_source (module Symbolizer);
  register_brancher_source ();
  Project.Input.register_loader name loader

type headless = bool option
type mode = Info.mode option

let bool_of_headless = function
  | Some x -> x
  | None -> Bap_ida_config.is_headless

let find_path = function
  | None -> Bap_ida_config.ida_path
  | Some p -> p

module Cmdline = struct

  module Headless = struct
    type t = headless

    let parser = function
      | "true"  -> `Ok (Some true)
      | "false" -> `Ok (Some false)
      | "auto"  -> `Ok None
      | _ -> `Error "headless should be one of true | false | auto"

    let printer fmt t = Format.fprintf fmt "%s"
        (match t with
         | Some x -> sprintf "%b" x
         | None -> "auto")

    let t = Config.converter parser printer None
  end

  module Mode = struct
    type t = mode

    let parser = function
      | "32" -> `Ok (Some `m32)
      | "64" -> `Ok (Some `m64)
      | "auto" -> `Ok None
      | _ -> `Error "mode should be one of 32 | 64 | auto"

    let printer fmt t = Format.fprintf fmt "%s"
        (match t with
         | Some `m32 -> "32"
         | Some `m64 -> "64"
         | None -> "auto")

    let t = Config.converter parser printer None
  end

  let () =
    let () = Config.manpage [
        `S "DESCRIPTION";
        `P "This plugin provides rooter, symbolizer and reconstuctor services.";
        `P "If IDA instance is found on the machine, or specified by a
        user, it will be queried for the specified information.";
        `S "SEE ALSO";
        `P "$(b,bap-ida)(3), $(b,regular)(3),$(b,bap-plugin-byteweight)(1), $(b,bap-plugin-objdump)(1)"
      ] in

    let path =
      let doc = "Path to IDA directory." in
      Config.(param (some string) "path" ~doc) in
    let headless =
      let doc = "Use headless curses based IDA." in
      Config.(param Headless.t "headless" ~default:None ~doc) in
    let mode =
      let doc = "Specify IDA mode." in
      Config.(param Mode.t "mode" ~default:None ~doc) in
    Config.when_ready (fun {Config.get=(!)} ->
        let is_headless = bool_of_headless !headless in
        let ida_path = find_path !path in
        match Info.create ida_path is_headless with
        | Ok info -> Bap_ida_service.register info !mode; main ()
        | Error e ->
          warning "%S. Service not registered." (Error.to_string_hum e))
end
