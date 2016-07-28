open Core_kernel.Std
open Format
open Bap.Std
open Bap_plugins.Std
open Mc_options
include Self()

exception Bad_user_input
exception Bad_insn of mem * int * int
exception Convert_imm of string
exception Create_mem of Error.t
exception No_input
exception Unknown_arch
exception Trailing_data of int

module Program(Conf : Mc_options.Provider) = struct
  open Conf
  module Dis = Disasm_expert.Basic

  let bad_insn addr state mem start =
    let stop = Addr.(Dis.addr state - addr |> to_int |> ok_exn) in
    raise (Bad_insn (Dis.memory state, start, stop))

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
    try bytes |> List.map ~f:map |> String.concat |> Scanf.unescaped
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
      | "0x" ->  to_binary ~map:escape_0x input
      | x -> to_binary ~map:prepend_slash_x input

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
      printf "%#x@\n" len

  let print_insn insn_formats insn =
    let insn = Insn.of_basic insn in
    List.iter insn_formats ~f:(fun fmt ->
        Insn.with_printer fmt (fun () ->
            printf "%a@." Insn.pp insn))

  let bil_of_insn lift mem insn =
    match lift mem insn with
    | Ok bil -> bil
    | Error e -> [Bil.special @@ sprintf "Lifter: %s" @@
                  Error.to_string_hum e]

  let print_bil lift mem insn =
    let bil = bil_of_insn lift mem in
    List.iter options.bil_formats ~f:(fun fmt ->
        printf "%s@." (Bil.to_bytes ~fmt (bil insn)))

  let print_bir lift mem insn =
    let bil = bil_of_insn lift mem insn in
    let bs = Blk.from_insn (Insn.of_basic ~bil insn) in
    List.iter options.bir_formats ~f:(fun fmt ->
        printf "%s" @@ String.concat ~sep:"\n"
          (List.map bs ~f:(Blk.to_bytes ~fmt)))

  let print arch mem insn =
    let module Target = (val target_of_arch arch) in
    print_insn_size options.show_insn_size mem;
    print_insn options.insn_formats insn;
    print_bil Target.lift mem insn;
    print_bir Target.lift mem insn;
    if options.show_kinds then print_kinds insn

  let main () =
    let arch = match Arch.of_string options.arch with
      | None -> raise Unknown_arch
      | Some arch -> arch in
    let size = match Arch.addr_size arch with
      | `r32 -> ":32"
      | `r64 -> ":64" in
    let addr = Addr.of_string (options.addr ^ size) in
    let input = read_input options.src in
    let mem = create_memory arch input addr in
    let backend = options.disassembler in
    Dis.with_disasm ~backend (options.arch) ~f:(fun dis ->
        let bytes = Dis.run dis mem ~return:ident ~init:0
            ~stop_on:[`Valid] ~invalid:(bad_insn addr)
            ~hit:(fun state mem insn bytes ->
                print arch mem insn;
                if options.only_one then Dis.stop state bytes
                else Dis.step state (bytes + Memory.length mem)) in
        match String.length input - bytes with
        | 0 -> Or_error.return ()
        | _ when options.only_one -> Or_error.return ()
        | n -> raise (Trailing_data n))
end

let format_info get_fmts =
  get_fmts () |> List.map ~f:fst3 |> String.concat ~sep:", "

let print_data_formats data_type =
  let print = Bap_format_printer.run `writers in
  match data_type with
  | `insn -> print (module Insn)
  | `bil  -> print (module Bil)
  | `bir  -> print (module Blk)

module Cmdline = struct
  open Cmdliner
  open Bap_cmdline_terms

  let arch =
    let doc = "Target architecture" in
    Arg.(value & opt string "x86_64" & info ["arch"] ~docv:"ARCH" ~doc)

  let show_kinds =
    let doc = "Output instruction kinds." in
    Arg.(value & flag & info ["show-kinds"] ~doc)

  let show_insn_size =
    let doc = "Output recognized instruction length" in
    Arg.(value & flag & info ["show-size"] ~doc)

  let list_formats_types = [
    "insn", `insn;
    "inst", `insn;
    "bil",  `bil;
    "bir",  `bir;
  ]
  let list_formats_doc = sprintf
      "Print available formats for the given data $(i,TYPE). \
       The $(i,TYPE) must be %s" @@
    Arg.doc_alts_enum list_formats_types
  let list_formats_option = "list-formats"
  let list_formats =
    Arg.(value & opt (some (enum list_formats_types)) None &
         info [list_formats_option] ~doc:list_formats_doc)

  let insn_formats =
    let doc = sprintf
        "Print instructions, using specified format $(docv). \
         $(docv) can be %s. Defaults to `asm'." @@
      format_info Insn.available_writers in
    Arg.(value & opt_all ~vopt:"pretty" string [] &
         info ["show-inst"; "show-insn"] ~doc)

  let bil_formats =
    let doc = sprintf
        "Output BIL code. Optional value specifies format \
         and can be %s. Defaults to `pretty`, i.e., in a BIL \
         concrete syntax" @@ format_info Stmt.available_writers in
    Arg.(value & opt_all ~vopt:"pretty" string [] &
         info ["show-bil"] ~doc)

  let bir_formats =
    let doc = sprintf
        "Output for each instruction in particular. Accepted \
         values are %s" @@ format_info Blk.available_writers in
    Arg.(value & opt_all ~vopt:"pretty" string [] &
         info ["show-bir"] ~doc)

  let addr =
    let doc = "Specify an address of first byte" in
    Arg.(value & opt  string "0x0" &  info ["addr"] ~doc)

  let only_one =
    let doc = "Stop after the first instruction is decoded" in
    Arg.(value & flag & info ["only-one"] ~doc)

  let create a b c d e f g h i j =
    Mc_options.Fields.create a b c d e f g h i j

  let src =
    let doc = "String to disassemble. If not specified read stdin" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"DATA" ~doc)

  let program () =
    let doc = "BAP machine instruction playground" in
    let man = [
      `S "SYNOPSIS";
      `Pre "
 $(b,$mname) [PLUGIN OPTION]... --list-formats
 $(b,$mname) [OPTION]... DATA";
      `S "DESCRIPTION";
      `P "Disassemble a string of bytes. This is the BAP machine \
          code playground. It is intended to mimic a subset of \
          llvm-mc functionality using the BAP disassembly backend.";
      `S "OPTIONS";
      `I ("$(b,--list-formats)=$(i,TYPE)", list_formats_doc);
    ] @ Bap_cmdline_terms.common_loader_options
      @ [
        `S "EXAMPLES";
        `P "The following hex representations are supported:"; `Noblank;
        `Pre "
         0x31 0xd2 0x48 0xf7 0xf3
         \\\\x31\\\\xd2\\\\x48\\\\xf7\\\\xf3
         31 d2 48 f7 f3
         31d248f7f3";
        `I ("INPUT: Supplied via stdin or on the command-line",
            "echo \"0x31 0xd2 0x48 0xf7 0xf3\" | \
             bap-mc  --show-inst --show-bil");
        `S "SEE ALSO";
        `P "llvm-mc"] in
    Term.(const create $(disassembler ()) $src $addr $only_one $arch $show_insn_size
          $insn_formats $bil_formats $bir_formats $show_kinds),
    Term.info "bap-mc" ~doc ~man ~version

  let exitf n =
    kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter

  let parse argv =
    match Cmdliner.Term.eval_peek_opts ~argv list_formats with
    | Some (Some typ),_ -> print_data_formats typ; exit 0
    | _ -> match Term.eval ~argv (program ()) ~catch:false with
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
  Log.start ();
  let argv = Bap_plugin_loader.run Sys.argv in
  try match Or_error.(Cmdline.parse argv >>= start) with
    | Ok () -> exit 0
    | Error err -> exitf 64 "%s\n" Error.(to_string_hum err)
  with
  | Bad_user_input ->
    exitf 65 "Could not parse: malformed input"
  | No_input -> exitf 66 "Could not read from stdin"
  | Unknown_arch ->
    exitf 64 "Unknown architecture. Supported architectures:\n%s" @@
    String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string
  | Trailing_data 1 -> exitf 65 "the last byte wasn't disassembled"
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
