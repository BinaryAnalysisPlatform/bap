open Core_kernel.Std
open Or_error
open Format
open Bap.Std
open Bap_plugins.Std
open Options

exception Bad_user_input
exception Bad_insn of mem * int * int
exception Convert_imm of string
exception Create_mem of Error.t
exception No_input
exception Unknown_arch
exception Trailing_data of int

module Program(Conf : Options.Provider) = struct
  open Conf
  module Dis = Disasm_expert.Basic

  let no_disassembly state (start_addr, boff) =
    let mem = Dis.memory state in
    let addr = Addr.((Dis.addr state) - start_addr) in
    let stop = (Addr.to_int addr |> ok_exn) in
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

  let create_memory arch s addr =
    let endian = Arch.endian arch in
    Memory.create endian addr @@
    Bigstring.of_string s |> function
    | Ok r -> r
    | Error e -> raise (Create_mem e)

  let print_kinds insn =
    Dis.Insn.kinds insn |>
    List.map ~f:sexp_of_kind |>
    List.iter ~f:(printf "%a@." Sexp.pp)

  let print_insn_size should_print mem =
    if should_print then
      let len = Memory.length mem in
      printf "%#x\n" len

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
    | Ok bil -> bil
    | Error e -> [Bil.special @@ sprintf "Lifter: %s" @@
                  Error.to_string_hum e]

  let pp_sexp fmt x =
    Sexp.pp fmt (sexp_of_bil x)

  let string_of_list pp bil =
    List.iter bil ~f:(fun stmt ->
        pp str_formatter stmt;
        pp_print_newline str_formatter ());
    flush_str_formatter ()


  let string_of_bil = function
    | `bil -> asprintf "%a" Bil.pp
    | `adt -> string_of_list Stmt.pp_adt
    | `sexp -> asprintf "%a" pp_sexp
    | `binprot -> Binable.to_string (module Bil)
    | #Bil_piqi.fmt as fmt -> Bil_piqi.string_of_bil fmt

  let print_bil lift mem insn =
    let bil = bil_of_insn lift mem in
    List.iter options.bil_formats ~f:(fun fmt ->
        printf "%s@." (string_of_bil fmt (bil insn)))

  let string_of_bir = function
    | `binprot -> Binable.to_string (module Blk)
    | `sexp -> fun blk -> asprintf "%a" Sexp.pp (Blk.sexp_of_t blk)
    | `bir -> asprintf "%a" Blk.pp

  let print_bir lift mem insn =
    let bil = bil_of_insn lift mem insn in
    let bs = Blk.from_insn (Insn.of_basic ~bil insn) in
    List.iter options.bir_formats ~f:(fun fmt ->
        printf "%s" @@ String.concat ~sep:"\n"
          (List.map bs ~f:(string_of_bir fmt)))

  let make_print arch mem insn =
    let module Target = (val target_of_arch arch) in
    print_insn_size options.show_insn_size mem;
    print_insn options.insn_formats insn;
    print_bil Target.lift mem insn;
    print_bir Target.lift mem insn;
    if options.show_kinds then print_kinds insn

  let check max_insn counter = match max_insn with
    | None -> true
    | Some max_insn -> counter < max_insn

  let step print state mem insn (addr, counter) =
    if (check options.max_insn counter) then (
      print mem insn;
      Dis.step state (Dis.addr state, counter+1)
    ) else Dis.stop state (Dis.addr state, counter)

  let main () =
    let arch = match Arch.of_string options.arch with
      | None -> raise Unknown_arch
      | Some arch -> arch in
    let extension = match Arch.addr_size arch with
      | `r32 -> ":32"
      | `r64 -> ":64" in
    let addr = Addr.of_string (options.addr ^ extension) in
    let print =
      make_print arch in
    let input = read_input options.src in
    Dis.create ~backend:"llvm" (Arch.to_string arch) >>= fun dis ->
    let invalid state mem (r_addr, off) =
      no_disassembly state (r_addr, off) in
    let pos, dis_insn_count  =
      Dis.run dis ~return:(fun x -> x)
        ~stop_on:[`Valid] ~invalid
        ~hit:(step print) ~init:(addr, 0)
        (create_memory arch input addr) in
    let bytes_disassembled = Addr.(pos - addr) |> Addr.to_int |> ok_exn in
    let len = String.length input in
    match options.max_insn with
    | None ->
      if bytes_disassembled <> len then
        raise (Trailing_data (len - bytes_disassembled));
      return 0
    | _ -> return 0
end


module Cmdline = struct
  open Cmdliner

  let arch =
    let doc = "Target architecture" in
    Arg.(value & opt string "x86_64" & info ["arch"] ~docv:"ARCH" ~doc)

  let show_kinds =
    let doc = "Output instruction kinds." in
    Arg.(value & flag & info ["show-kinds"] ~doc)

  let show_insn_size =
    let doc =
      "Output recognized opcode length (including potential data)\
       as in the number of bytes EIP is incremented upon having\
       executed said opcode."
    in
    Arg.(value & flag & info ["show-size"] ~doc)

  let insn_formats =
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

  let bil_formats =
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

  let bir_formats =
    let formats = [
      "bir", `bir;
      "sexp", `sexp;
      "binprot", `binprot
    ] in
    let doc = sprintf
        "Output for each instruction in particular. Accepted \
         values are %s" @@ Arg.doc_alts_enum formats in
    Arg.(value & opt_all ~vopt:`bir (enum formats) [] &
         info ["show-bir"] ~doc)

  let addr =
    let doc = "Specify an address of first byte, as though \
               the instructions occur at a certain address, \
               and accordingly interpreted. Be careful that \
               you appropriately use 0x prefix for hex and \
               leave it without for decimal." in
    Arg.(value & opt  string "0x0" &  info ["addr"] ~doc)

  let max_insns =
    let doc = "Specify a number of instructions to disassemble.\
               Good for ensuring that only one instruction is ever\
               lifted or disassembled from a byte blob. Default is all" in
    Arg.(value & opt (some int) None & info ["max-insns"] ~doc)


  let create a b c d e f g h i =
    Options.Fields.create a b c d e f g h i

  let src =
    let doc = "String to disassemble. If not specified read stdin" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"STRING" ~doc)

  let program =
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
           bap-mc  --show-inst --show-bil");
      `S "SEE ALSO";
      `P "llvm-mc"] in
    Term.(pure create $src $addr $max_insns $arch $show_insn_size
          $insn_formats $bil_formats $bir_formats $show_kinds),
    Term.info "bap-mc" ~doc ~man ~version:Config.pkg_version

  let exitf n =
    kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter

  let parse () =
    match Term.eval program ~catch:false with
    | `Ok opts -> Ok opts
    | `Error `Parse -> exit 64
    | `Error _ -> exit 2
    | _ -> exit 1
end

let exitf n =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter

let start options =
  let module Program = Program(struct
      let options = options
    end) in
  Program.main ()

let _main : unit =
  Plugins.load ();
  try match Cmdline.parse () >>= start with
    | Ok _ -> exit 0
    | Error err -> exitf 64 "%s\n" Error.(to_string_hum err)
  with
  | Bad_user_input ->
    exitf 65 "Could not parse: malformed input"
  | No_input -> exitf 66 "Could not read from stdin"
  | Unknown_arch ->
    exitf 64 "Unknown architecture. Supported architectures:\n%s" @@
    String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string
  | Trailing_data left ->
    exitf 65 "%d bytes were left non disassembled" left
  | Create_mem err ->
    exitf 65 "Unable to create a memory: %a" Error.pp err
  | Bad_insn (mem,boff,stop)->
    let dump = Memory.hexdump mem in
    let line = boff / 16 in
    let pos off = line * 77 + (off mod 16) * 3 + 9 in
    dump.[pos boff] <- '(';
    dump.[pos stop] <- ')';
    exitf 66 "Invalid instruction at offset %d:\n%s" boff dump
