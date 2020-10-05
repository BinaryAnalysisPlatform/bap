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

# SETTING ARCHITECHTURE

The target architecture is controlled by several groups of options
that can not be used together:

- $(b,arch);
- $(b,target) and $(b,encoding);
- $(b,triple), $(b,backend), $(b,cpu), $(b,bits), and $(b,order).

The $(b,arch) option provides the least control but is easiest to
use. It relies on the dependency-injection mechanism and lets the
target support packages (plugins that implement support for the given
architecture) do their best to guess the target and encoding that
matches the provided name. Use the common names for the architecture
and it should work. You can use the $(b,bits) and $(b,order) options
to give more hints to the target support packages. They default to
$(b,32) and $(b,little) correspondingly.

The $(b,target) and $(b,encoding) provides precise control over the
selection of the target and the encoding that is used to represent
machine instructions. The $(b,encoding) field can be omitted and will
be deduced from the target. Use $(b, bap list targets) and
$(b, bap list encodings) to get the list of supported targets and
encodings respectivly.

Finally, the $(b,triple), $(b,backend), $(b,cpu),... group of options
provides the full control over the disassembler backend and bypasses
the dependency-injection mechanism to pass the specified options
directly to the corresponding backends. This enables disassembling of
targets and encodings that are not yet supported by BAP. The meanings
of the options totally depend on the selected $(b,backend) and they
are passed as is to the corresponding arguments of the
$(b,Disasm_expert.Basic.create) function. The $(b,bits) and $(b,order)
defaults to $(b,32) and $(b,little) corresondingly and are used to
specify the number of bits in the target's addresses and the order of
bytes in the word. This group of options is useful during the
implementation and debugging of new targets and thus is reserved for
experts. Note, when this group is used the semantics of the instructions
will not be provided as it commonly requires the target specification.
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
  | Target_must_be_unknown
  | Encoding_must_be_unknown
  | Triple_must_not_be_set
  | Arch_must_not_be_set
  | Backend_must_not_be_set
  | Cpu_must_not_be_set
  | Bits_must_not_be_set
  | Order_must_not_be_set

type Extension.Error.t += Fail of error

type output = [
  | `insn
  | `bil
  | `bir
  | `sema
  | `kinds
  | `size
  | `invalid
] [@@deriving compare]

type target =
  | Target of {
      name : Theory.Target.t;
      encoding : Theory.Language.t;
    }
  | Triple of {
      name : string;
      backend : string option;
      cpu: string option;
      order : endian;
      bits : int;
    }

let outputs = [
  `insn;
  `bil;
  `bir;
  `sema;
  `kinds;
  `size;
  `invalid;
]

let fail err = Error (Fail err)

let enabled = "enabled"

module Spec = struct
  open Extension

  let input = Command.arguments Type.string
  let file = Command.argument Type.file

  let language = Type.define
      ~parse:(Theory.Language.read ~package:"bap")
      ~print:Theory.Language.to_string
      Theory.Language.unknown

  let target = Type.define
      ~parse:(Theory.Target.get ~package:"bap")
      ~print:Theory.Target.to_string
      Theory.Target.unknown

  let order = Type.enum [
      "big", BigEndian;
      "little", LittleEndian;
    ]

  let arch = Command.parameter Type.(some string) "arch"
      ~aliases:["a"]
      ~doc:"The target architecture."

  let target = Command.parameter target "target"
      ~aliases:["t"]
      ~doc:"The target name."

  let encoding = Command.parameter language "encoding"
      ~aliases:["e"]
      ~doc:"The target encoding."

  let triple = Command.parameter Type.(some string) "triple"
      ~doc:"The target triple."

  let cpu = Command.parameter Type.(some string) "cpu"
      ~doc:"The target CPU (used with triple)."

  let bits = Command.parameter Type.(some int) "bits"
      ~doc:"The number of bits in the address \
            (used with triple or arch)"

  let order = Command.parameter Type.(some order) "order"
      ~doc: "The order of bytes in the target's word \
             (used with triple or arch)."

  let outputs =
    let name_of_output = function
      | `insn -> "insn"
      | `bil  -> "bil"
      | `bir  -> "bir"
      | `sema -> "sema"
      | `kinds -> "kinds"
      | `size -> "size"
      | `invalid -> "invalid" in

    let as_flag = function
      | `insn | `bil | `bir -> ["pretty"]
      | `sema -> ["all-slots"]
      | `kinds | `size | `invalid -> [enabled] in

    let doc = function
      | `insn -> "Print the decoded instruction."
      | `bil -> "Print the BIL code."
      | `bir -> "Print the IR."
      | `sema -> "Print the knowledge base."
      | `kinds -> "Print semantics tags associated with instruction."
      | `size -> "Print the instruction size."
      | `invalid -> "Print invalid instructions." in

    let name s = "show-" ^ name_of_output s in
    Extension.Command.dictionary ~doc ~as_flag outputs
      Type.(list string) name

  let base =
    let doc = "Specify an address of first byte" in
    Command.parameter ~aliases:["b"] ~doc Type.string "address"

  let only_one =
    let doc = "Stop after the first instruction is decoded" in
    Command.flag ~doc "only-one"

  let stop_on_error = Command.flag "stop-on-errors"
      ~doc:"Stop disassembling on the first error and report it"

  let backend =
    let doc = "The disassembling backend (used with triple)." in
    Command.parameter ~doc Type.(some string) "backend"

  let loader =
    Extension.Command.parameter
      ~doc:"Use the specified loader .
          Use the loader `raw' to load unstructured files"
      Extension.Type.(string =? "llvm") "loader"

end

module Dis = Disasm_expert.Basic

let bad_insn addr state start =
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

let endian = function
  | Triple {order} -> order
  | Target {name=t} ->
    if Theory.Endianness.(equal le) (Theory.Target.endianness t)
    then LittleEndian
    else BigEndian

let create_memory arch data base =
  Memory.create (endian arch) base @@
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
  let provide_target unit label = function
    | Triple _ -> KB.return ()
    | Target {name=target; encoding} ->
      KB.provide Theory.Unit.target unit target >>= fun () ->
      if Theory.Language.is_unknown encoding
      then KB.return ()
      else KB.provide Theory.Label.encoding label encoding in
  KB.Object.create Theory.Program.cls >>= fun code ->
  KB.Symbol.intern "unit" Theory.Unit.cls >>= fun unit ->
  provide_target unit code arch >>= fun () ->
  KB.provide Theory.Label.unit code (Some unit) >>= fun () ->
  KB.provide Memory.slot code (Some mem) >>= fun () ->
  KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
  code

let lift arch mem insn =
  match KB.run Theory.Program.cls (new_insn arch mem insn) KB.empty with
  | Ok (code,_) -> Ok (KB.Value.get Theory.Semantics.slot code)
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

let equal_output = [%compare.equal: output]

let is_enabled = function
  | [opt] -> String.equal enabled opt
  | _ -> false

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

let bits = function
  | Target {name=t} -> Theory.Target.bits t
  | Triple {bits} -> bits

let parse_base arch base =
  Result.map_error ~f:(function
      | Invalid_argument str -> Fail (Invalid_base str)
      | exn -> Fail (Invalid_base (Exn.to_string exn))) @@
  Result.try_with @@ fun () ->
  Word.create (Bitvec.of_string base) (bits arch)

let create_disassembler spec =
  Result.map_error ~f:(fun err -> Fail (Disassembler_failed err)) @@
  match spec with
  | Target {name; encoding} -> Dis.lookup name encoding
  | Triple {name; cpu; backend} -> Dis.create ?backend ?cpu name

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
      | (`kinds|`size|`invalid),[] -> Ok ()
      | (`kinds|`size|`invalid),[opt]
        when String.equal enabled opt -> Ok ()
      | `kinds,_ -> Error (No_formats_expected "kinds")
      | `size,_ -> Error (No_formats_expected "size")
      | `invalid,_ -> Error (No_formats_expected "invalid")
      | `sema,_ ->
        (* no validation right now, since the knowledge introspection
           is not yet implemented *)
        Ok ())


let print_invalid _pos =

  Format.printf "<invalid>@\n"

let run ?(only_one=false) ?(stop_on_error=false) dis arch mem formats =
  let show_invalid = is_enabled (formats `invalid) in
  Dis.run dis mem
    ~init:0
    ~return:Result.return
    ~stop_on:[`Valid]
    ~invalid:(fun state _ pos ->
        if show_invalid then print_invalid pos;
        if stop_on_error
        then bad_insn (Memory.min_addr mem) state pos
        else Dis.step state pos)
    ~hit:(fun state mem insn bytes ->
        print arch mem insn formats >>= fun () ->
        if only_one then Dis.stop state bytes
        else Dis.step state (bytes + Memory.length mem))

let check_invariants xs =
  List.concat_map xs ~f:(fun (pred,props) ->
      if pred then props else []) |>
  Result.all_unit

let check check t error =
  if not (check t) then fail error else Ok ()

let target_must_be_unknown t =
  check Theory.Target.is_unknown t Target_must_be_unknown

let encoding_must_be_unknown t =
  check Theory.Language.is_unknown t Encoding_must_be_unknown

let triple_must_not_be_set x =
  check Option.is_none x Triple_must_not_be_set

let arch_must_not_be_set x =
  check Option.is_none x Arch_must_not_be_set

let backend_must_not_be_set x =
  check Option.is_none x Backend_must_not_be_set

let cpu_must_not_be_set x =
  check Option.is_none x Cpu_must_not_be_set

let bits_must_not_be_set x =
  check Option.is_none x Bits_must_not_be_set

let order_must_not_be_set x =
  check Option.is_none x Order_must_not_be_set



let compute_target provide =
  let extract_target =
    let open KB.Syntax in
    KB.Object.scoped Theory.Unit.cls @@ fun unit ->
    KB.Object.scoped Theory.Program.cls @@ fun label ->
    provide unit >>= fun () ->
    KB.provide Theory.Label.unit label (Some unit) >>= fun () ->
    Theory.Label.target label >>= fun name ->
    KB.collect Theory.Label.encoding label >>| fun encoding ->
    Target {name; encoding} in
  let result = Toplevel.var "target-and-encoding" in
  Toplevel.put result extract_target;
  Toplevel.get result

let target_of_arch arch bits order =
  let bits = match bits with
    | None -> 32L
    | Some bits -> Int64.of_int bits in
  let is_little = match order with
    | Some BigEndian -> false
    | _ -> true in
  let make_spec =
    let open Ogre.Syntax in
    Ogre.sequence [
      Ogre.provide Image.Scheme.arch arch;
      Ogre.provide Image.Scheme.bits bits;
      Ogre.provide Image.Scheme.is_little_endian is_little;
    ] in
  let spec = match Ogre.exec make_spec Ogre.Doc.empty with
    | Error err ->
      failwithf "compute_target: failed to build a spec: %s"
        (Error.to_string_hum err) ()
    | Ok doc -> doc in
  compute_target @@ fun unit ->
  KB.provide Image.Spec.slot unit spec

let make_triple ?(bits=32) ?(order=BigEndian) ?backend ?cpu name =
  Triple {name; backend; cpu; bits; order}

let make_target target encoding =
  if Theory.Language.is_unknown encoding
  then compute_target @@ fun unit ->
    KB.provide Theory.Unit.target unit target
  else Target {name=target; encoding}

let parse_arch
    arch
    target encoding
    triple cpu backend
    bits order =
  check_invariants [
    Option.is_some arch, [
      target_must_be_unknown target;
      triple_must_not_be_set triple;
    ];
    not (Theory.Target.is_unknown target), [
      arch_must_not_be_set arch;
      triple_must_not_be_set triple;
    ];
    Option.is_some triple, [
      target_must_be_unknown target;
      encoding_must_be_unknown encoding;
      arch_must_not_be_set arch;
    ];
    Theory.Target.is_unknown target, [
      encoding_must_be_unknown encoding;
    ];
    Option.is_none triple, [
      cpu_must_not_be_set cpu;
      backend_must_not_be_set backend;
    ];
    Option.is_none triple && Option.is_none arch, [
      bits_must_not_be_set bits;
      order_must_not_be_set order;
    ]
  ] >>| fun () -> match arch,triple with
  | None,None ->
    if Theory.Target.is_unknown target
    then target_of_arch "x86-64" None None
    else make_target target encoding
  | Some arch,None -> target_of_arch arch bits order
  | None,Some triple -> make_triple ?bits ?order ?backend ?cpu triple
  | Some _, Some _ ->
    failwith "parse_arch: unchecked invariant"

let () = Extension.Command.(begin
    declare ~doc:mc_man "mc"
      Spec.(args
            $arch
            $target $encoding
            $triple $cpu $backend
            $bits $order
            $base $only_one $stop_on_error $input $outputs)
  end) @@ fun arch
    target encoding
    triple cpu backend
    bits order
    base only_one stop_on_error input outputs _ctxt ->
  validate_formats outputs >>= fun () ->
  parse_arch arch target encoding triple cpu backend bits order >>= fun arch ->
  read_input input >>= fun data ->
  parse_base arch base >>= fun base ->
  create_memory arch data base >>= fun mem ->
  create_disassembler arch >>= fun dis ->
  let formats = formats outputs in
  run ~only_one ~stop_on_error dis arch mem formats >>= fun bytes ->
  Dis.close dis;
  match String.length data - bytes with
  | 0 -> Ok ()
  | _ when only_one -> Ok ()
  | n -> fail (Trailing_data n)

let () = Extension.Command.(begin
    declare ~doc:objdump_man "objdump"
      Spec.(args $loader $stop_on_error $file $outputs)
  end) @@ fun loader stop_on_error input outputs _ctxt ->
  validate_formats outputs >>= fun () ->
  let formats = formats outputs in
  match Image.create ~backend:loader input with
  | Error err -> Error (Fail (Loader_failed err))
  | Ok (img,_warns) ->
    let target = compute_target @@ fun unit ->
      KB.provide Image.Spec.slot unit (Image.spec img) in
    create_disassembler target >>= fun dis ->
    Image.memory img |>
    Memmap.to_sequence |>
    Seq.filter_map ~f:(fun (mem,data) ->
        Option.some_if
          (Value.is Image.code_region data) mem) |>
    Seq.map ~f:(fun mem ->
        run ~stop_on_error dis target mem formats >>= fun _bytes ->
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
  | Target_must_be_unknown
  | Triple_must_not_be_set
  | Arch_must_not_be_set ->
    "The target, triple, and arch options could not be used together"
  | Encoding_must_be_unknown ->
    "The encoding option requires the target option"
  | Backend_must_not_be_set ->
    "The backend option requires the triple option"
  | Cpu_must_not_be_set ->
    "The CPU option requires the triple option"
  | Bits_must_not_be_set | Order_must_not_be_set ->
    "The bits and order parameters are only accepted with arch or \
     triple and are not allowed when the target is specified"


let () = Extension.Error.register_printer @@ function
  | Fail err -> Some (string_of_failure err)
  | _ -> None
