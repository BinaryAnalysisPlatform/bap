let mc_man = {|
# DESCRIPTION

Disassembles a (human readable) string of bytes. This command is the
BAP machine code playground, which is intended to mimic a subset of
llvm-mc functionality, using the BAP disassembly backend. The main
use case is to explore various information associated with the
dissasembled instruction.

# EXAMPLES

The following input formats are supported:

```
    0x31 0xd2 0x48 0xf7 0xf3
    \\\\x31\\\\xd2\\\\x48\\\\xf7\\\\xf3
    31 d2 48 f7 f3
    31d248f7f3";
```

|}

let objdump_man = {|
# DESCRIPTION

Disassembles and prints a binary using the linear sweep algorithm.
This command is a sibling to the $(b,mc) command, except
that it takes a binary file as an input. If the binary contains only
raw code (i.e., no meta information), then use the $(b,raw) loader.

# EXAMPLES

```
  bap objdump /bin/ls --show-asm
  bap objdump ./code --loader=raw
```
|}

open Core_kernel
open Format
open Regular.Std
open Bap.Std
open Bap_plugins.Std
open Bap_core_theory
open Bap_main

open Result.Monad_infix

type error =
  | Bad_user_input
  | Bad_insn of mem * int * int
  | Create_mem of Error.t
  | No_input
  | Unknown_arch
  | Invalid_base of string
  | Trailing_data of int
  | Inconsistency of KB.conflict
  | Unknown_format of string * string * string list
  | No_formats_expected of string
  | Disassembler_failed of Error.t
  | Loader_failed of Error.t

type Extension.Error.t += Fail of error

type output = [
  | `insn
  | `bil
  | `bir
  | `sema
  | `kinds
  | `size
] [@@deriving compare]

let outputs = [
  `insn;
  `bil;
  `bir;
  `sema;
  `kinds;
  `size;
]

let fail err = Error (Fail err)

module Spec = struct
  open Extension

  let input = Command.arguments Type.string
  let file = Command.argument Type.file

  let arch_type = Type.define
      ~parse:(fun s -> match Arch.of_string s with
          | None -> invalid_arg "unknown architecture"
          | Some arch -> arch)
      ~print:Arch.to_string
      `x86_64

  let arch =
    let doc = "Target architecture" in
    Command.parameter ~doc arch_type "arch"


  let outputs =
    let name_of_output = function
      | `insn -> "insn"
      | `bil  -> "bil"
      | `bir  -> "bir"
      | `sema -> "sema"
      | `kinds -> "kinds"
      | `size -> "size" in

    let as_flag = function
      | `insn | `bil | `bir -> ["pretty"]
      | `sema -> ["all-slots"]
      | `kinds | `size -> [] in

    let doc = function
      | `insn -> "Print the decoded instruction."
      | `bil -> "Print the BIL code."
      | `bir -> "Print the IR."
      | `sema -> "Print the knowledge base."
      | `kinds -> "Print semantics tags associated with instruction."
      | `size -> "Print the instruction size." in

    let name s = "show-" ^ name_of_output s in
    Extension.Command.dictionary ~doc ~as_flag outputs
      Type.(list string) name

  let base =
    let doc = "Specify an address of first byte" in
    Command.parameter ~aliases:["b"] ~doc Type.string "address"

  let only_one =
    let doc = "Stop after the first instruction is decoded" in
    Command.flag ~doc "only-one"

  let backend =
    let doc = "Specify the disassembler backend" in
    Command.parameter ~doc Type.(some string) "backend"

  let loader =
    Extension.Command.parameter
      ~doc:"Use the specified loader .
          Use the loader `raw' to load unstructured files"
      Extension.Type.(string =? "llvm") "loader"

end

module Dis = Disasm_expert.Basic

let bad_insn addr state _ start =
  let stop = Addr.(Dis.addr state - addr |> to_int |> ok_exn) in
  fail (Bad_insn (Dis.memory state, start, stop))

let escape_0x =
  String.substr_replace_all ~pattern:"0x" ~with_:"\\x"

let prepend_slash_x x = "\\x" ^ x

(** [to_binary ?escape s] make a binary string from ascii
    representation, (e.g., "\x01\x02..."). Apply optional
    escape function for each byte *)
let to_binary ?(map=ident) s =
  let seps = [' '; ','; ';'] in
  let separated = List.exists seps ~f:(String.mem s) in
  let bytes = if separated
    then String.split_on_chars ~on:seps s
    else List.init (String.length s / 2) ~f:(fun n ->
        String.slice s (n*2) (n*2+2)) in
  try List.map bytes ~f:map |>
      String.concat |>
      Scanf.unescaped |>
      Result.return
  with _ -> fail Bad_user_input

let read_input input =
  let input = match input with
    | [] | ["-"] -> In_channel.input_line In_channel.stdin
    | s -> Some (String.concat s) in
  match input with
  | None -> fail No_input
  | Some input -> match String.prefix input 2 with
    | "" | "\n" -> fail No_input
    | "\\x" -> to_binary input
    | "0x" ->  to_binary ~map:escape_0x input
    | _ -> to_binary ~map:prepend_slash_x input

let create_memory arch data base =
  let endian = Arch.endian arch in
  Memory.create endian base @@
  Bigstring.of_string data |> function
  | Ok r -> Ok r
  | Error e -> fail (Create_mem e)

let print_kinds formats insn =
  List.iter formats ~f:(fun _ ->
      Dis.Insn.kinds insn |>
      List.map ~f:sexp_of_kind |>
      List.iter ~f:(printf "%a@." Sexp.pp))

let new_insn arch mem insn =
  let open KB.Syntax in
  KB.Object.create Theory.Program.cls >>= fun code ->
  KB.provide Arch.slot code (Some arch) >>= fun () ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let lift arch mem insn =
  match KB.run Theory.Program.cls (new_insn arch mem insn) KB.empty with
  | Ok (code,_) -> Ok (KB.Value.get Theory.Program.Semantics.slot code)
  | Error conflict -> fail (Inconsistency conflict)

let print_insn_size formats mem =
  List.iter formats ~f:(fun _fmt ->
      printf "%#x@\n" (Memory.length mem))

let print_insn insn_formats insn =
  List.iter insn_formats ~f:(fun fmt ->
      Insn.with_printer fmt (fun () ->
          printf "%a@." Insn.pp insn))

let print_bil formats insn =
  let bil = Insn.bil insn in
  List.iter formats ~f:(fun fmt ->
      printf "%s@." (Bytes.to_string @@ Bil.to_bytes ~fmt bil))

let print_bir formats insn  =
  let bs = Blk.from_insn insn in
  List.iter formats ~f:(fun fmt ->
      printf "%s" @@ String.concat ~sep:"\n"
        (List.map bs ~f:(fun b -> Bytes.to_string @@ Blk.to_bytes ~fmt b)))

let print_sema formats sema = match formats with
  | [] -> ()
  | ["all-slots"] -> printf "%a@\n" KB.Value.pp sema
  | some_slots ->
    let pp = KB.Value.pp_slots some_slots in
    printf "%a@\n" pp sema

let equal_output x y = compare_output x y = 0
let formats outputs kind =
  match List.Assoc.find outputs kind ~equal:equal_output with
  | None -> []
  | Some fmts -> fmts

let print arch mem code formats =
  lift arch mem code >>| fun insn ->
  print_insn_size (formats `size) mem;
  print_insn (formats `insn) insn;
  print_bil (formats `bil) insn;
  print_bir (formats `bir) insn;
  print_sema (formats `sema) insn;
  print_kinds (formats `kinds) code

let parse_base arch base =
  Result.map_error ~f:(function
      | Invalid_argument str -> Fail (Invalid_base str)
      | exn -> Fail (Invalid_base (Exn.to_string exn))) @@
  Result.try_with @@ fun () ->
  Word.create (Bitvec.of_string base)
    (Size.in_bits (Arch.addr_size arch))


let create_disassembler ?(backend="llvm") arch =
  Dis.create ~backend (Arch.to_string arch) |>
  Result.map_error ~f:(fun err -> Fail (Disassembler_failed err))


let module_of_kind = function
  | `insn -> "Bap.Std.Insn"
  | `bil -> "Bap.Std.Bil"
  | `bir -> "Bap.Std.Blk"


let validate_module kind formats =
  let name = module_of_kind kind in
  Data.all_writers () |>
  List.find_map ~f:(fun (modname,fmts) ->
      Option.some_if (String.equal modname name) fmts) |> function
  | None ->
    failwithf "Unable to find printers for module %s" name ()
  | Some fmts ->
    let fmts = List.map fmts ~f:(fun (n,_,_) -> n) in
    let provided = Set.of_list (module String) fmts in
    Result.all_unit @@
    List.map formats ~f:(fun fmt ->
        if Set.mem provided fmt then Ok ()
        else Error (Unknown_format (name,fmt,fmts)))

let validate_formats formats =
  Result.map_error ~f:(fun err -> Fail err) @@
  Result.all_unit @@
  List.map formats ~f:(function
      | (`insn|`bil|`bir) as kind,fmts ->
        validate_module kind fmts
      | (`kinds|`size),[] -> Ok ()
      | `kinds,_ -> Error (No_formats_expected "kinds")
      | `size,_ -> Error (No_formats_expected "size")
      | `sema,_ ->
        (* no validation right now, since the knowledge introspection
           is not yet implemented *)
        Ok ())

let run ?(only_one=false) dis arch mem formats =
  Dis.run dis mem
    ~init:0
    ~return:Result.return
    ~stop_on:[`Valid]
    ~invalid:(bad_insn (Memory.min_addr mem))
    ~hit:(fun state mem insn bytes ->
        print arch mem insn formats >>= fun () ->
        if only_one then Dis.stop state bytes
        else Dis.step state (bytes + Memory.length mem))

let () = Extension.Command.(begin
    declare ~doc:mc_man "mc"
      Spec.(args $arch $base $backend $only_one $input $outputs)
  end) @@ fun arch base backend only_one input outputs _ctxt ->
  validate_formats outputs >>= fun () ->
  parse_base arch base >>= fun base ->
  read_input input >>= fun data ->
  create_memory arch data base >>= fun mem ->
  create_disassembler ?backend arch >>= fun dis ->
  let formats = formats outputs in
  run ~only_one dis arch mem formats >>= fun bytes ->
  Dis.close dis;
  match String.length data - bytes with
  | 0 -> Ok ()
  | _ when only_one -> Ok ()
  | n -> fail (Trailing_data n)

let () = Extension.Command.(begin
    declare ~doc:objdump_man "objdump"
      Spec.(args $backend $loader $file $outputs)
  end) @@ fun backend loader input outputs _ctxt ->
  validate_formats outputs >>= fun () ->
  let formats = formats outputs in
  match Image.create ~backend:loader input with
  | Error err -> Error (Fail (Loader_failed err))
  | Ok (img,_warns) ->
    let arch = Image.arch img in
    create_disassembler ?backend arch >>= fun dis ->
    Image.memory img |>
    Memmap.to_sequence |>
    Seq.filter_map ~f:(fun (mem,data) ->
        Option.some_if
          (Value.is Image.code_region data) mem) |>
    Seq.map ~f:(fun mem ->
        run dis arch mem formats >>= fun _bytes ->
        Ok ()) |>
    Seq.to_list |>
    Result.all_unit


let format_info get_fmts =
  get_fmts () |> List.map ~f:fst3 |> String.concat ~sep:", "


let string_of_failure = function
  | Inconsistency conflict ->
    Format.asprintf "Lifters failed with a conflict: %a"
      KB.Conflict.pp conflict
  | Bad_user_input -> "Could not parse: malformed input"
  | No_input -> "No input was received"
  | Unknown_arch ->
    sprintf "Unknown architecture. Supported architectures:\n%s" @@
    String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string
  | Trailing_data 1 -> "the last byte wasn't disassembled"
  | Trailing_data left ->
    sprintf "%d bytes were left non disassembled" left
  | Create_mem err ->
    Format.asprintf "Unable to create a memory: %a" Error.pp err
  | Invalid_base msg ->
    sprintf "Failed to parse the base address: %s" msg
  | Disassembler_failed err ->
    Format.asprintf "Failed to create the disassembler backend: %a"
      Error. pp err
  | Bad_insn (mem,boff,stop)->
    let dump = Memory.hexdump mem |> Bytes.of_string in
    let line = boff / 16 in
    let pos off = line * 77 + (off mod 16) * 3 + 9 in
    Bytes.set dump (pos boff) '(';
    Bytes.set dump (pos stop) ')';
    sprintf "Invalid instruction at offset %d:\n%s"
      boff (Bytes.to_string dump)
  | Unknown_format (mname,fmt,fmts) ->
    let pp_sep = Format.pp_print_newline in
    Format.asprintf "@[<v2>Unknown printer %s for %s, expecting: %a@]"
      fmt mname Format.(pp_print_list ~pp_sep pp_print_string) fmts
  | No_formats_expected name ->
    sprintf "--show-%s doesn't expect any formats yet" name
  | Loader_failed err ->
    Format.asprintf "Failed to unpack the file: %a" Error.pp err


let () = Extension.Error.register_printer @@ function
  | Fail err -> Some (string_of_failure err)
  | _ -> None
