open Core_kernel.Std open Or_error open OUnit2
open Bap.Std
open Disasm

exception Bad_user_input
exception No_disassembly
exception Convert_imm_exn of string
exception Create_mem_exn
exception Stdin_exn

let create_memory addr s =
  Memory.create LittleEndian Addr.(of_int64 addr) @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error _ -> raise Create_mem_exn

let print_kinds insn =
  let output = Insn.kinds insn
               |> List.map ~f:(fun kind ->
                   Sexp.to_string_hum (Insn.Kind.sexp_of_t kind))
               |> String.concat ~sep:", " in
  printf "%-4s;; %s\n" " " output

let print_insn insn width o_reg_format o_imm_format =
  let open Op in
  let init = [Sexp.Atom (Insn.name insn)] in
  let res =
    Insn.ops insn
    |> Array.fold ~init ~f:(fun l x -> match x with
        | Reg reg ->
          if String.(o_reg_format = "code") then
            Sexp.Atom ("r:" ^ Int.to_string (Reg.code reg)) :: l else
            Sexp.Atom (Reg.name reg) :: l
        | Imm imm ->
          if String.(o_imm_format = "dec") then
            let v = match Imm.to_int imm with
              | Some x -> x
              | None -> raise @@ Convert_imm_exn (Imm.to_string imm) in
            Sexp.Atom (Printf.sprintf "%d" v) :: l else
            Sexp.Atom (Imm.to_string imm) :: l
        | Fmm fmm ->
          Sexp.Atom (Fmm.to_string fmm) :: l) in
  let s = Sexp.to_string @@ Sexp.List (List.rev res) in
  printf "%-4s%-*s" " " width s

(* Note: Insn.asm returns string with 8 character padding, Strip it to keep
 * things consistent *)
let print_asm insn f_inst =
  let s = String.strip @@ Insn.asm insn in
  if f_inst
  then printf "; %s" s
  else printf "%-4s%s" " " s

let print_disasm width f_asm f_inst f_kinds o_reg_format o_imm_format
    state mem insn disasm =
  if f_kinds then print_kinds insn;
  if f_inst then print_insn insn width o_reg_format o_imm_format;
  if f_asm then print_asm insn f_inst;
  if (f_asm || f_inst) then print_newline ();
  Basic.step state ()

(* Convert strings to binary string representation "\x01\x02..." *)
let to_bin_str s f =
  try String.split ~on:' ' s |> List.map ~f |> String.concat |> Scanf.unescaped
  with Scanf.Scan_failure _ -> raise Bad_user_input

let disasm s o_arch f_asm f_inst f_kinds o_reg_format o_imm_format =
  let input_src =
    match s with
    | "" -> In_channel.input_line In_channel.stdin
    | _ -> Some s in
  Disasm.Basic.create ~backend:"llvm" o_arch >>= fun dis ->
  let input = match input_src with
    | Some input ->
      begin match String.prefix input 2 with
        | "" | "\n" -> exit 0
        | "\\x" -> let f = ident in to_bin_str input f
        | "0x" -> let f = String.substr_replace_all ~pattern:"0x" ~with_:"\\x"
          in to_bin_str input f
        | _ -> let f x = "\\x" ^ x in to_bin_str input f
      end
    | None -> raise Stdin_exn in
  let mem_of_input = create_memory 0x0L input in
  (* arm instructions tend to be longer, so increase the printed width *)
  let width = if o_arch = "arm" then 65 else 50 in
  let hit =
    print_disasm width f_asm f_inst f_kinds o_reg_format o_imm_format in
  let invalid state disasm = raise No_disassembly in
  Disasm.Basic.run dis ~return:ident ~stop_on:[`Valid] ~invalid ~hit ~init:()
    mem_of_input;
  return ()

open Cmdliner

let o_arch =
  let doc = "Target architecture (x86_64 or arm)." in
  Arg.(value & opt string "x86_64" & info ["arch"] ~docv:"ARCH" ~doc)

let o_reg_format =
  let doc = "Register format (code or name)." in
  Arg.(value & opt string "name" & info ["reg-format"] ~docv:"REG_FORMAT" ~doc)

let o_imm_format =
  let doc = "Imm format (hex or dec)." in
  Arg.(value & opt string "hex" & info ["imm-format"] ~docv:"IMM_FORMAT" ~doc)

let f_kinds =
  let doc = "Output instruction kinds." in
  Arg.(value & flag & info ["show-kinds"] ~doc)

let f_inst =
  let doc = "Output BAP instruction disassembly." in
  Arg.(value & flag & info ["show-inst"] ~doc)

let f_asm =
  let doc = "Output disassembly." in
  Arg.(value & flag & info ["show-asm"] ~doc)

let hex_str =
  let doc = "String to disassemble (if not specified on stdin)." in
  Arg.(value & pos 0 string "" & info [] ~docv:"STRING" ~doc)

let cmd =
  let doc = "manual page for bap-mc 1.0" in
  let man = [
    `S "DESCRIPTION";
    `I ("OVERVIEW: Disassemble a string of hex bytes",
        "This is the BAP machine code playground. It is intended to mimic a
        subset of llvm-mc functionality using the BAP disassembly backend.");
    `S "EXAMPLES";
    `P "Three hex representations are supported:"; `Noblank;
    `I (".BR", " 0x31 0xd2 0x48 0xf7 0xf3"); `Noblank;
    `I (".BR", " \\\\x31\\\\xd2\\\\x48\\\\xf7\\\\xf3"); `Noblank;
    `I (".BR", " 31 d2 48 f7 f3");
    `I ("INPUT: Supplied via stdin or on the command-line",
        "echo \"0x31 0xd2 0x48 0xf7 0xf3\" | bap-mc  --show-inst --show-asm");
    `S "SEE ALSO";
    `P "$(llvm-mc)"] in
  Term.(pure disasm $ hex_str $ o_arch $ f_asm $ f_inst $ f_kinds
        $ o_reg_format $ o_imm_format),
  Term.info "bap-mc" ~doc ~man ~version:"1.0"

let () =
  Plugins.load ();
  let err = Format.std_formatter in
  try match Term.eval cmd ~catch:false ~err with
    | `Error `Parse -> exit 64
    | `Error _ -> exit 1
    | _ -> exit 0
  with e ->
    let fin s n = print_endline s; exit n in
    match e with
    | Bad_user_input ->
      fin "Could not parse: malformed input" 65
    | Convert_imm_exn imm ->
      fin (sprintf "Unable to convert Imm hex value [%s] to int" imm) 1
    | Create_mem_exn ->
      fin "Internal error: cannot create memory for dissasembly backend" 1
    | Stdin_exn ->
      fin "Could not read from stdin" 1
    | No_disassembly ->
      fin (sprintf "Invalid instruction encountered, disassembly stopped") 1
    | _ -> fin "Could not disassemble input" 1
