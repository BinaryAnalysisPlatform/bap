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
    let version = "1.0.0"
  end)

module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  module Factory : sig
    val register : string -> t source -> unit
  end
end

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
    Data.Cache.digest ~namespace:"ida" "%s" (Digest.file path) in
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

let register_source (module T : Target) =
  let source =
    let open Project.Info in
    let extract file arch = Or_error.try_with ~backtrace:true (fun () ->
        extract file arch |> T.of_blocks) in
    Stream.merge file arch ~f:extract in
  T.Factory.register name source


type perm = [`code | `data] [@@deriving sexp]
type section = string * perm * int * (int64 * int)
[@@deriving sexp]

type image = string * addr_size * section list [@@deriving sexp]

module Img = Data.Make(struct
    type t = image
    let version = "1.0.0"
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
    ~script:(request "loader")
    ~parser:read_image


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

let int64_to_word arch = 
  let addr_size = Arch.addr_size arch in 
  (Word.of_int64 ~width:(Size.in_bits addr_size)) 

(* copied from lib/bap_disasm/bap_disasm_brancher.ml *)
let kind_of_dests = function 
  | xs when List.for_all xs ~f:(fun (_,x) -> x = `Fall) -> `Fall 
  | xs -> if List.exists  xs ~f:(fun (_,x) -> x = `Jump) 
    then `Jump 
    else `Cond 

let print_addr_with_dests addr dests = 
  Format.printf "%x: %a\n" 
    (Int64.to_int_exn addr) 
    Sexp.pp (Brancher.sexp_of_dests dests) 

(* brancher_info is a list of 3-tuples of the form 
 * (addr, jump of normal flow (fall) type, jumps of other types) *) 
type brancher_info = (int64 * int64 option * int64 list) list 
[@@deriving sexp] 

module BrancherInfo = Data.Make(struct 
    type t = brancher_info 
    let version = "0.1" 
  end) 

let read_brancher_info name = 
  In_channel.with_file name ~f:(fun ch -> 
      Sexp.input_sexp ch |> brancher_info_of_sexp) 

let load_brancher_info = 
  Command.create 
    `python 
    ~script:(request "brancher") 
    ~parser:read_brancher_info 

let handle_normal_flow flow arch = 
  let (!) = (int64_to_word arch) in 
  match flow with 
  | Some fall -> [Some !fall, `Fall] 
  | None -> [] 

let handle_other_flows flows default_dests arch = 
  let (!) = (int64_to_word arch) in 
  let helper acc dest_addr = 
    let default_edge = ListLabels.assoc_opt 
        (Some !dest_addr) 
        default_dests 
    in 
    match default_edge with 
    (* If dest_addr is also found by the default BIL brancher, 
     * we reuse the edge type *) 
    | Some kind -> (Some !dest_addr, kind)::acc 
    (* Else we use a heuristic to guess the edge type *) 
    | None -> (Some !dest_addr, (kind_of_dests default_dests))::acc 
  in 
  List.fold flows ~init:[] ~f:helper 

let get_resolve_fun file arch = 
  let id = Data.Cache.digest ~namespace:"ida-brancher" "%s" 
      (Digest.file file) in 
  let info = match BrancherInfo.Cache.load id with 
    | Some i -> i 
    | None -> 
      let i = Ida.with_file file load_brancher_info in 
      BrancherInfo.Cache.save id i; 
      i in 
  let resolve mem insn = 
    let addr = Word.to_int64_exn (Memory.min_addr mem) in 
    let b = List.find 
        info 
        ~f:(fun (a,_,_) -> a = addr) in 
    match b with 
    | Some (_,normal_flow,other_flows) -> 
      let default_brancher = Brancher.of_bil arch in 
      let default_dests = Brancher.resolve default_brancher mem insn in 
      (handle_normal_flow normal_flow arch) 
      @ (handle_other_flows other_flows default_dests arch) 
    | None -> 
      [] in 
  resolve 

let register_brancher_source () = 
  let source = 
    let create_brancher file arch = Or_error.try_with (fun () -> 
        Brancher.create (get_resolve_fun file arch)) in 
    Project.Info.(Stream.merge file arch ~f:create_brancher) in 
  Brancher.Factory.register name source

let checked ida_path is_headless =
  Bap_ida_check.(check_headless is_headless >>= fun () -> check_path ida_path)

let main () =
  register_source (module Rooter);
  register_source (module Symbolizer);
  register_source (module Reconstructor);
  register_brancher_source ();
  Project.Input.register_loader name loader

type headless = bool option
type mode = [ `m32 | `m64 ] option

let ida_mode is_headless mode =
  let map = function
    | `m32 when is_headless -> `idal
    | `m64 when is_headless -> `idal64
    | `m32 -> `idaq
    | `m64 -> `idaq64 in
  Option.value_map mode ~default:None ~f:(fun x -> Some (map x))

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
        let ida_mode = ida_mode is_headless !mode in
        let ida_path = find_path !path in
        match checked ida_path is_headless with
        | Ok () ->
          Bap_ida_service.register ida_path ida_mode is_headless; main ()
        | Error e ->
          error "%S. Service not registered." (Error.to_string_hum e))
end
