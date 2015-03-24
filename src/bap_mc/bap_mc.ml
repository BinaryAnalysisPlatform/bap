open Core_kernel.Std
open Or_error
open OUnit2
open Bap.Std
open Bap_plugins.Std

module Disasm = Disasm_expert.Basic
module Insn = Disasm.Insn

exception Bad_user_input
exception No_disassembly of mem * int * int
exception Convert_imm_exn of string
exception Create_mem_exn
exception Stdin_exn

let no_disassembly state boff =
  let mem = Disasm.memory state in
  let addr = Disasm.addr state in
  let stop = Addr.to_int addr |> ok_exn in
  raise (No_disassembly (mem, boff, stop))

let create_memory arch addr s =
  let width = match Arch.of_string arch with
    | Some arch -> Arch.addr_size arch |> Size.to_bits
    | None -> eprintf "Warning: unknown arch, assuming 32 bitness\n";
      32 in
  Memory.create LittleEndian Addr.(of_int64 ~width addr) @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error _ -> raise Create_mem_exn

let print_kinds insn =
  let output = Insn.kinds insn
               |> List.map ~f:(fun kind ->
                   Sexp.to_string_hum (Disasm.sexp_of_kind kind))
               |> String.concat ~sep:", " in
  printf "%-4s;; %s\n" " " output

let print_insn insn width o_reg_format o_imm_format =
  let open Disasm.Op in
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

let rec dinsns_to_insn_list = function
  | [(_, None)] 
  | [] -> []
  | (_, None) :: restInsns -> dinsns_to_insn_list restInsns
  | (_, Some insn) :: restInsns -> insn :: (dinsns_to_insn_list restInsns)

let insns_to_bil_list insns = 
  let insns = (dinsns_to_insn_list insns) in
  (List.map insns Bap.Std.Insn.bil) 

let print_bilsexp_of_insns insns =
  let bil_list = insns_to_bil_list insns in
  let f x= Sexp.to_string_hum (sexp_of_bil x) in
  let string_list = (List.map bil_list f) in
  List.iter string_list print_string;;

let print_pb_of_insns insns =
  let bil_list = insns_to_bil_list insns in
  let f x = (Bil_piqi.pb_of_stmts (Bap.Std.Insn.bil x)) in
  List.iter (List.map (dinsns_to_insn_list insns) ~f:f) ~f:print_string

let print_json_of_insns insns =
  let bil_list = insns_to_bil_list insns in
  let f x = (Bil_piqi.json_of_stmts (Bap.Std.Insn.bil x)) in
  List.iter (List.map (dinsns_to_insn_list insns) ~f:f) print_string

let print_xml_of_insns insns =
  let bil_list = insns_to_bil_list insns in
  let f x = Bil_piqi.xml_of_stmts (Bap.Std.Insn.bil x) in
  List.iter (List.map (dinsns_to_insn_list insns) ~f:f) print_string

(* Note: Insn.asm returns string with 8 character padding, Strip it to keep
 * things consistent *)
let print_asm insn f_inst =
  let s = String.strip @@ Insn.asm insn in
  if f_inst
  then printf "; %s" s
  else printf "%-4s%s" " " s

let print_disasm arch width f_asm f_inst f_bil f_bil_to_pb f_bil_to_json f_bil_to_xml f_kinds o_reg_format o_imm_format
    state mem insn off =
  if f_kinds then print_kinds insn;
  if f_inst then print_insn insn width o_reg_format o_imm_format;
  if f_bil then print_bilsexp_of_insns (linear_sweep_exn arch mem);
  if f_bil_to_pb then print_pb_of_insns (linear_sweep_exn arch mem);
  if f_bil_to_json then print_json_of_insns (linear_sweep_exn arch mem);
  if f_bil_to_xml then print_xml_of_insns (linear_sweep_exn arch mem);
  if f_asm then print_asm insn f_inst;
  if (f_asm || f_inst) then print_newline ();
  Disasm.step state (Disasm.addr state |> Addr.to_int |> ok_exn)

(* Convert strings to binary string representation "\x01\x02..." *)
let to_bin_str s f =
  let seps = [' '; ','; ';'] in
  let separated = List.exists seps ~f:(String.mem s) in
  let bytes = if separated
    then String.split_on_chars ~on:seps s
    else List.init (String.length s / 2) ~f:(fun n ->
        String.slice s (n*2) (n*2+2)) in
  try bytes |> List.map ~f |> String.concat |> Scanf.unescaped
  with Scanf.Scan_failure _ -> raise Bad_user_input

let disasm s o_arch f_asm f_inst
	   f_bil f_bil_to_pb f_bil_to_json f_bil_to_xml
	   f_kinds o_reg_format o_imm_format =
  let input_src =
    match s with
    | "" -> In_channel.input_line In_channel.stdin
    | _ -> Some s in
  Disasm.create ~backend:"llvm" o_arch >>= fun dis ->
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
  (* arm instructions tend to be longer, so increase the printed width *)
  let width = if o_arch = "arm" then 65 else 50 in
  let arch = if o_arch="arm" then `arm
	     else if o_arch="x86" then `x86
	     else if o_arch="x86_64" || o_arch="x86-64" then `x86_64
	     else `x86 in
  let hit =
    print_disasm arch width f_asm f_inst f_bil
		 f_bil_to_pb f_bil_to_json f_bil_to_xml
		 f_kinds o_reg_format o_imm_format in
  let invalid state mem off = no_disassembly state off in
  let mem = create_memory o_arch 0x0L input in
  let _pos : int =
    Disasm.run dis ~return:ident ~stop_on:[`Valid] ~invalid ~hit ~init:0
      mem in
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

let f_bil = 
  let doc = "Output unadulterated BIL for given hex." in
  Arg.(value & flag & info ["show-bil"] ~doc)

let f_bil_pb =
  let doc = "Output protobuf of BIL for given hex." in
  Arg.(value & flag & info ["show-pb"] ~doc)

let f_bil_json =
  let doc = "Output json of BIL for given hex." in
  Arg.(value & flag & info ["show-json"] ~doc)

let f_bil_xml =
  let doc = "Output xml of BIL for given hex." in
  Arg.(value & flag & info ["show-xml"] ~doc)

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
    `I (".BR", " 31d248f7f3");
    `I ("INPUT: Supplied via stdin or on the command-line",
        "echo \"0x31 0xd2 0x48 0xf7 0xf3\" | bap-mc  --show-inst --show-asm");
    `S "SEE ALSO";
    `P "$(llvm-mc)"] in
  Term.(pure disasm $ hex_str $ o_arch $ f_asm $ f_inst
	$ f_bil $ f_bil_pb $ f_bil_json $ f_bil_xml
	$ f_kinds $ o_reg_format $ o_imm_format),
  Term.info "bap-mc" ~doc ~man ~version:"1.0"

let () =
  Plugins.load ();
  let err = Format.std_formatter in
  try match Term.eval cmd ~catch:false ~err with
    | `Ok Ok () -> exit 0
    | `Ok Error err ->
      eprintf "%s\n" Error.(to_string_hum err); exit 2
    | `Error `Parse -> exit 64
    | `Error _ -> exit 2
    | _ -> exit 1

  with e ->
    let fin n s = eprintf "%s\n" s; exit n in
    match e with
    | Bad_user_input ->
      fin 65 "Could not parse: malformed input"
    | Convert_imm_exn imm ->
      sprintf "Unable to convert Imm hex value [%s] to int" imm |>
      fin 1
    | Create_mem_exn ->
      fin 1 "Internal error: cannot create memory for dissasembly backend"
    | Stdin_exn ->
      fin 1 "Could not read from stdin"
    | No_disassembly (mem,boff,stop)->
      let dump = Memory.hexdump mem in
      let line = boff / 16 in
      let pos off = line * 77 + (off mod 16) * 3 + 9 in
      dump.[pos boff] <- '(';
      dump.[pos stop] <- ')';
      sprintf "Invalid instruction at offset %d:\n%s" boff dump |>
      fin 1
    | _ -> fin 1 "Could not disassemble input"
