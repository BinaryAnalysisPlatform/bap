open Core_kernel.Std
open Or_error
open Format
open Bap.Std
open Bap_plugins.Std

module Dis = Disasm_expert.Basic

exception Bad_user_input
exception Bad_insn of mem * int * int
exception Convert_imm of string
exception Create_mem of Error.t
exception No_input
exception Unknown_arch
exception Can't_lift of Error.t
exception Trailing_data of int

let no_disassembly state boff =
  let mem = Dis.memory state in
  let addr = Dis.addr state in
  let stop = Addr.to_int addr |> ok_exn in
  raise (Bad_insn (mem, boff, stop))

let escape_0x =
  String.substr_replace_all ~pattern:"0x" ~with_:"\\x"

let prepend_slash_x x = "\\x" ^ x

(** [to_binary ?escape s] make a binary string from ascii
    representation, (e.g., "\x01\x02..."). Apply optional
    escape function for each byte *)
let to_binary ?(escape=ident) s =
  let seps = [' '; ','; ';'] in
  let separated = List.exists seps ~f:(String.mem s) in
  let bytes = if separated
    then String.split_on_chars ~on:seps s
    else List.init (String.length s / 2) ~f:(fun n ->
        String.slice s (n*2) (n*2+2)) in
  try bytes |> List.map ~f:escape |> String.concat |> Scanf.unescaped
  with Scanf.Scan_failure _ -> raise Bad_user_input

let read_input input =
  let input = match input with
    | None -> In_channel.input_line In_channel.stdin
    | Some s -> Some s in
  match input with
  | None -> raise No_input
  | Some input -> match String.prefix input 2 with
    | "" | "\n" -> exit 0
    | "\\x" -> to_binary input
    | "0x" ->  to_binary ~escape:escape_0x input
    | x -> to_binary ~escape:prepend_slash_x input

let create_memory arch s =
  let width = Arch.addr_size arch |> Size.to_bits in
  let endian = Arch.endian arch in
  Memory.create endian Addr.(of_int ~width 0) @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error e -> raise (Create_mem e)

let print_kinds insn =
  Dis.Insn.kinds insn |>
  List.map ~f:sexp_of_kind |>
  List.iter ~f:(printf "%a@." Sexp.pp)

let print_insn insn_formats insn =
  let insn = Insn.of_basic insn in
  List.iter insn_formats ~f:(function
      | `asm -> printf "%s@." @@ Insn.asm insn
      | `adt -> printf "%a@." Insn.pp_adt insn
      | `sexp ->
        printf "(%s %s)@."
          (Insn.name insn)
          List.(Insn.ops insn |> Array.to_list >>| Op.to_string |>
                String.concat ~sep:" "))

let bil_of_insn lift mem insn =
  match lift mem insn with
  | Error e -> raise (Can't_lift e)
  | Ok bil -> bil

let pp_sexp fmt x =
  Sexp.pp fmt (sexp_of_bil x)

let string_of_list pp bil =
  List.iter bil ~f:(fun stmt ->
      pp str_formatter stmt;
      pp_print_newline str_formatter ());
  flush_str_formatter ()

let string_of_bil = function
  | `pb -> Bil_piqi.pb_of_stmts
  | `json -> Bil_piqi.json_of_stmts
  | `xml -> Bil_piqi.xml_of_stmts
  | `bil -> asprintf "%a" Bil.pp
  | `adt -> string_of_list Stmt.pp_adt
  | `sexp -> asprintf "%a" pp_sexp
  | `binprot -> Binable.to_string (module Bil)

let print_bil lift bil_formats mem insn =
  let bil = bil_of_insn lift mem in
  List.iter bil_formats ~f:(fun fmt ->
      printf "%s@." (string_of_bil fmt (bil insn)))

let make_print lift insn_fmt bil_fmt show_kinds mem insn =
  print_insn insn_fmt insn;
  print_bil lift bil_fmt mem insn;
  if show_kinds then print_kinds insn

let step print state mem insn _ =
  print mem insn;
  Dis.step state (Dis.addr state |> Addr.to_int |> ok_exn)

let disasm src arch show_insn show_bil show_kinds =
  let arch = match Arch.of_string arch with
    | None -> raise Unknown_arch
    | Some arch -> arch in
  let module Target = (val target_of_arch arch) in
  let print = make_print Target.lift show_insn show_bil show_kinds in
  let input = read_input src in
  Dis.create ~backend:"llvm" (Arch.to_string arch) >>= fun dis ->
  let invalid state mem off = no_disassembly state off in
  let pos =
    Dis.run dis ~return:ident
      ~stop_on:[`Valid] ~invalid ~hit:(step print) ~init:0
      (create_memory arch input) in
  if pos <> String.length input then
    raise (Trailing_data (String.length input - pos));
  return ()

module Cmdline = struct
  open Cmdliner

  let arch =
    let doc = "Target architecture" in
    Arg.(value & opt string "x86_64" & info ["arch"] ~docv:"ARCH" ~doc)

  let show_kinds =
    let doc = "Output instruction kinds." in
    Arg.(value & flag & info ["show-kinds"] ~doc)

  let show_insn =
    let formats = [
      "asm", `asm;
      "adt", `adt;
      "sexp", `sexp;
    ] in
    let doc = sprintf
        "Print instructions, using specified format $(docv). \
         $(docv) can be %s. Defaults to `asm'." @@
      Arg.doc_alts_enum formats in
    Arg.(value & opt_all ~vopt:`asm (enum formats) [] &
         info ["show-inst"; "show-insn"] ~doc)

  let show_bil =
    let formats = [
      "bil", `bil;
      "pb", `pb;
      "json", `json;
      "xml", `xml;
      "sexp", `sexp;
      "binprot", `binprot;
      "adt", `adt;
    ] in
    let doc = sprintf
        "Output BIL code. Optional value specifies format \
         and can be %s. Defaults to `bil`, i.e., in a BIL \
         concrete syntax" @@ Arg.doc_alts_enum formats in
    Arg.(value & opt_all ~vopt:`bil (enum formats) [] &
         info ["show-bil"] ~doc)

  let src =
    let doc = "String to disassemble. If not specified read stdin" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"STRING" ~doc)

  let cmd main =
    let doc = "BAP machine instruction playground" in
    let man = [
      `S "DESCRIPTION";
      `I ("OVERVIEW",
          " Disassemble a string of bytes. This is the BAP machine \
           code playground. It is intended to mimic a subset of \
           llvm-mc functionality using the BAP disassembly backend.");
      `S "EXAMPLES";
      `P "Four hex representations are supported:"; `Noblank;
      `I (".BR", " 0x31 0xd2 0x48 0xf7 0xf3"); `Noblank;
      `I (".BR", " \\\\x31\\\\xd2\\\\x48\\\\xf7\\\\xf3"); `Noblank;
      `I (".BR", " 31 d2 48 f7 f3");
      `I (".BR", " 31d248f7f3");
      `I ("INPUT: Supplied via stdin or on the command-line",
          "echo \"0x31 0xd2 0x48 0xf7 0xf3\" | \
           bap-mc  --show-inst --show-asm");
      `S "SEE ALSO";
      `P "llvm-mc"] in
    Term.(pure main $src $arch $show_insn $show_bil $show_kinds),
    Term.info "bap-mc" ~doc ~man ~version:Config.pkg_version

  let parse main = Term.eval (cmd main) ~catch:false
end

let exitf n =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter

let _main : unit =
  Plugins.load ();
  try match Cmdline.parse disasm with
    | `Ok Ok () -> exit 0
    | `Ok Error err -> exitf 12 "%s\n" Error.(to_string_hum err)
    | `Error `Parse -> exit 64
    | `Error _ -> exit 2
    | _ -> exit 1
  with
  | Bad_user_input ->
    exitf 65 "Could not parse: malformed input"
  | No_input -> exitf 1 "Could not read from stdin"
  | Unknown_arch ->
    exitf 64 "Unknown architecture. Supported architectures:\n%s" @@
    String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string
  | Trailing_data left ->
    exitf 1 "%d bytes were left non disassembled" left
  | Create_mem err ->
    exitf 65 "Unable to create a memory: %a" Error.pp err
  | Can't_lift err -> exitf 63 "Lifting failed: %a" Error.pp err
  | Bad_insn (mem,boff,stop)->
    let dump = Memory.hexdump mem in
    let line = boff / 16 in
    let pos off = line * 77 + (off mod 16) * 3 + 9 in
    dump.[pos boff] <- '(';
    dump.[pos stop] <- ')';
    exitf 1 "Invalid instruction at offset %d:\n%s" boff dump
