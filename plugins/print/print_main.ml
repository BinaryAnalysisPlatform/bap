open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std
open Format
open Bap_demangle.Std

open Option.Monad_infix

include Self()

let create_demangler = function
  | None -> ident
  | Some name ->
    Demanglers.available () |>
    List.find ~f:(fun d -> Demangler.name d = name) |> function
    | None ->
      error "Unknown demangler";
      info "Available demanglers are: %s" @@
      String.concat ~sep:", "
        List.(Demanglers.available () >>| Demangler.name);
      invalid_argf "Bad demangler option" ()
    | Some d -> Demangler.run d


let should_print = function
  | [] -> fun _ -> true
  | xs -> List.mem xs ~equal:String.equal


let find_section_for_addr memory addr =
  Memmap.lookup memory addr |> Seq.find_map ~f:(fun (_,v) ->
      Value.get Image.section v)

let bir memory sub =
  Term.get_attr sub address >>=
  find_section_for_addr memory

let sym memory (_,entry,_) =
  Block.addr entry |>
  find_section_for_addr memory

let sec_name memory fn sub =
  match fn memory sub with
  | None -> "bap.virtual"
  | Some name -> name

let print_symbols subs secs demangler fmts ppf proj =
  let demangle = create_demangler demangler in
  let symtab = Project.symbols proj in
  Symtab.to_sequence symtab |>
  Seq.filter ~f:(fun ((name,_,_) as fn) ->
      should_print subs name &&
      should_print secs (sec_name (Project.memory proj) sym fn)) |>
  Seq.iter ~f:(fun ((name,entry,_) as fn) ->
      List.iter fmts ~f:(function
          | `with_name ->
            fprintf ppf "%s " @@ demangle name
          | `with_addr ->
            let addr = Block.addr entry in
            fprintf ppf "%s " @@ Addr.string_of_value addr
          | `with_size ->
            let size = Symtab.span fn |>
                       Memmap.to_sequence |>
                       Seq.fold ~init:0 ~f:(fun size (mem,_) ->
                           Memory.length mem + size) in
            fprintf ppf "%2d " size);
      fprintf ppf "@\n")

let extract_program subs secs proj =
  let mem = Project.memory proj in
  Project.program proj |>
  Term.filter sub_t ~f:(fun sub ->
      should_print subs (Sub.name sub) &&
      should_print secs (sec_name mem bir sub))

let print_bir subs secs ppf proj =
  Text_tags.with_mode ppf "attr" ~f:(fun () ->
      Program.pp ppf (extract_program subs secs proj))

module Adt = struct
  let pr ch = Format.fprintf ch

  let pp_var ppf = Var.Io.print ~fmt:"adt" ppf
  let pp_exp ppf = Exp.Io.print ~fmt:"adt" ppf

  module Tid = struct
    let pp ppf tid = pr ppf "Tid(0x%a, %S)" Tid.pp tid (Tid.name tid)
  end

  let pp_seq pp_elem ch seq =
    let rec pp ch = function
      | [] -> ()
      | [x] -> pp_elem ch x
      | x :: xs -> pr ch "%a,@ %a" pp_elem x pp xs in
    pr ch "[%a]" pp (Seq.to_list seq)

  let pp_attr ch attr =
    pr ch {|Attr(%S,%S)|} (Value.tagname attr) (Value.to_string attr)

  let pp_attrs ch dict =
    pr ch "Attrs(%a)" (pp_seq pp_attr) (Dict.data dict)

  let pp_term name pp_self ch t =
    let tid = Term.tid t and dict = Term.attrs t in
    pr ch "%s(%a,@, %a,@, %a)"
      name Tid.pp tid pp_attrs dict pp_self t

  let pp_def_self ch t =
    let lhs = Def.lhs t and rhs = Def.rhs t in
    pr ch "%a,@ %a" pp_var lhs pp_exp rhs
  let pp_def = pp_term "Def" pp_def_self

  let pp_phi_value ch (tid,exp) =
    pr ch "(%a,%a)" Tid.pp tid pp_exp exp

  let pp_phi_self ch t =
    let lhs = Phi.lhs t and values = Phi.values t in
    pr ch "%a,@ Values(%a)" pp_var lhs
      (pp_seq pp_phi_value) values

  let pp_phi = pp_term "Phi" pp_phi_self

  let kind_of_jmp jmp = match Jmp.kind jmp with
    | Call _ -> "Call"
    | Goto _ -> "Goto"
    | Ret _ -> "Ret"
    | Int _ -> "Exn"

  let pp_label ch = function
    | Direct tid -> pr ch "Direct(%a)" Tid.pp tid
    | Indirect exp -> pr ch "Indirect(%a)" pp_exp exp

  let pp_call ch c =
    pr ch "(%a,%s)" pp_label (Call.target c)
      (Option.value_map (Call.return c) ~default:""
         ~f:(fun l -> Format.asprintf "%a" pp_label l))

  let pp_jmp_kind ch = function
    | Goto l | Ret l -> pr ch "%a" pp_label l
    | Int (n,r) -> pr ch "(%d,%a)" n Tid.pp r
    | Call c -> pr ch "%a" pp_call c

  let pp_jmp_self ch t =
    let cond = Jmp.cond t and kind = Jmp.kind t in
    pr ch "%a,@ %a" pp_exp cond pp_jmp_kind kind
  let pp_jmp ch jmp = pp_term (kind_of_jmp jmp) pp_jmp_self ch jmp

  let pp_array pp_elem ch xs = pp_seq pp_elem ch (Seq.of_array xs)

  let pp_blk_self ch t =
    pr ch "Phis(%a),@ Defs(%a),@ Jmps(%a)"
      (pp_seq pp_phi) (Term.enum phi_t t)
      (pp_seq pp_def) (Term.enum def_t t)
      (pp_seq pp_jmp) (Term.enum jmp_t t)

  let pp_blk = pp_term "Blk" pp_blk_self

  let intent = function
    | None -> ""
    | Some In -> "In()"
    | Some Out -> "Out()"
    | Some Both -> "Both()"

  let pp_arg_self ch t =
    let lhs = Arg.lhs t and rhs = Arg.rhs t and int = Arg.intent t in
    pr ch "%a,@ %a,@ %s" pp_var lhs pp_exp rhs (intent int)
  let pp_arg = pp_term "Arg" pp_arg_self

  let pp_sub_self ch t =
    pr ch "%S,@, Args(%a),@ Blks(%a)" (Sub.name t)
      (pp_seq pp_arg) (Term.enum arg_t t)
      (pp_seq pp_blk) (Term.enum blk_t t)

  let pp_sub = pp_term "Sub" pp_sub_self

  let pp_program_self ch t =
    pr ch "Subs(%a)" (pp_seq pp_sub) (Term.enum sub_t t)

  let pp_program = pp_term "Program" pp_program_self

  let pp_byte ppf byte =
    fprintf ppf "\\x%02x" (ok_exn (Word.to_int byte))

  let pp_sections ppf memmap =
    let sections = Memmap.to_sequence memmap |>
                   Seq.filter_map ~f:(fun (mem,attr) ->
                       match Value.get Image.section attr with
                       | Some name -> Some (name,mem)
                       | None -> None) in
    let pp_section ppf (name,mem) =
      fprintf ppf {|Section(%S, %s, "|}
        name (Addr.string_of_value (Memory.min_addr mem));
      Memory.iter ~word_size:`r8 mem ~f:(fun byte ->
          fprintf ppf "%a" pp_byte byte);
      fprintf ppf {|")|} in
    fprintf ppf "Sections(%a)" (pp_seq pp_section) sections

  let pp_memmap ppf memmap =
    let pp_region ppf mem =
      pr ppf "Region(%s,%s)"
        (Addr.string_of_value (Memory.min_addr mem))
        (Addr.string_of_value (Memory.max_addr mem)) in
    let pp_binding ppf (mem,attr) =
      pr ppf "Annotation(%a,@ %a)"
        pp_region mem pp_attr attr in
    fprintf ppf "Memmap(%a)"
      (pp_seq pp_binding) (Memmap.to_sequence memmap)


  let pp_project ppf proj =
    fprintf ppf "Project(%a,@ %a,@ %a,@ %a)"
      pp_attrs (Project.storage proj)
      pp_sections (Project.memory proj)
      pp_memmap (Project.memory proj)
      pp_program (Project.program proj)
end

let print_callgraph subs secs ppf proj =
  let prog = extract_program subs secs proj in
  fprintf ppf "%a@."
    Graphs.Callgraph.pp (Program.to_graph prog)

let print_bir_graph subs secs ppf proj =
  let prog = extract_program subs secs proj in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      fprintf ppf "%a@." Graphs.Ir.pp (Sub.to_cfg sub))

let pp_addr ppf a =
  Addr.pp_generic ~prefix:`none ~case:`lower ppf a

let setup_tabs ppf =
  pp_print_as ppf 50 "";
  pp_set_tab ppf () [@ocaml.warning "-3"]

let print_disasm pp_insn subs secs ppf proj =
  let memory = Project.memory proj in
  let syms = Project.symbols proj in
  pp_open_tbox ppf () [@ocaml.warning "-3"];
  setup_tabs ppf [@ocaml.warning "-3"];
  Memmap.filter_map memory ~f:(Value.get Image.section) |>
  Memmap.to_sequence |> Seq.iter ~f:(fun (mem,sec) ->
      Symtab.intersecting syms mem |>
      List.filter ~f:(fun (name,_,_) ->
          should_print subs name) |> function
      | [] -> ()
      | _ when not(should_print secs sec) -> ()
      | fns ->
        fprintf ppf "@\nDisassembly of section %s@\n" sec;
        List.iter fns ~f:(fun (name,entry,cfg) ->
            fprintf ppf "@\n%a: <%s>@\n" pp_addr (Block.addr entry) name;
            Graphlib.reverse_postorder_traverse (module Graphs.Cfg)
              ~start:entry cfg |> Seq.iter ~f:(fun blk ->
                  let mem = Block.memory blk in
                  fprintf ppf "%a:@\n" pp_addr (Memory.min_addr mem);
                  Block.insns blk |> List.iter ~f:(pp_insn ppf))));
  pp_close_tbox ppf () [@ocaml.warning "-3"]

let pp_bil fmt ppf (mem,insn) =
  let pp_bil ppf = Bil.Io.print ~fmt ppf in
  let addr = Memory.min_addr mem in
  fprintf ppf "%a: %s@\n%a@\n" pp_addr addr (Insn.asm insn)
    pp_bil (Insn.bil insn)

let pp_insn fmt ppf (mem,insn) =
  Memory.pp ppf mem;
  pp_print_tab ppf ()  [@ocaml.warning "-3"];
  Insn.Io.print ~fmt ppf insn;
  fprintf ppf "@\n"

let main attrs ansi_colors demangle symbol_fmts subs secs =
  let ver = version in
  let pp_syms =
    Data.Write.create ~pp:(print_symbols subs secs demangle symbol_fmts) () in
  Project.add_writer
    ~desc:"print symbol table" ~ver "symbols" pp_syms;
  let pp_bir = Data.Write.create ~pp:(print_bir subs secs) () in
  let pp_adt = Data.Write.create ~pp:Adt.pp_project () in

  List.iter attrs ~f:Text_tags.Attr.show;
  Text_tags.Attr.print_colors ansi_colors;
  Project.add_writer
    ~desc:"print program in IR" ~ver "bir" pp_bir;
  Project.add_writer
    ~desc:"print program IR in ADT format" ~ver "adt" pp_adt;
  let pp_callgraph =
    Data.Write.create ~pp:(print_callgraph subs secs) () in
  Project.add_writer ~ver "callgraph"
    ~desc:"print program callgraph in DOT format" pp_callgraph;
  let pp_cfg = Data.Write.create ~pp:(print_bir_graph subs secs) () in
  let pp_disasm_bil =
    Data.Write.create ~pp:(print_disasm (pp_bil "pretty") subs secs) () in
  let pp_disasm_bil_adt =
    Data.Write.create ~pp:(print_disasm (pp_bil "adt") subs secs) () in
  let pp_disasm_bil_sexp =
    Data.Write.create ~pp:(print_disasm (pp_bil "sexp") subs secs) () in
  let pp_disasm_asm =
    Data.Write.create ~pp:(print_disasm (pp_insn "asm") subs secs) () in
  let pp_disasm_adt =
    Data.Write.create ~pp:(print_disasm (pp_insn "adt") subs secs) () in
  let pp_disasm_decoded =
    Data.Write.create ~pp:(print_disasm (pp_insn "pretty") subs secs) () in
  let pp_disasm_sexp =
    Data.Write.create ~pp:(print_disasm (pp_insn "sexp") subs secs) () in
  Project.add_writer ~ver "cfg"
    ~desc:"print rich CFG for each procedure" pp_cfg;
  Project.add_writer ~ver "asm"
    ~desc:"print assembly instructions" pp_disasm_asm;
  Project.add_writer ~ver "asm.adt"
    ~desc:"print assembly instruction endcoded in ADT format" pp_disasm_adt;
  Project.add_writer ~ver "asm.decoded"
    ~desc:"print assembly instructions as it was decoded" pp_disasm_decoded;
  Project.add_writer ~ver "asm.sexp"
    ~desc:"print assembly instructions as it was decoded" pp_disasm_sexp;
  Project.add_writer ~ver "bil"
    ~desc:"print BIL instructions" pp_disasm_bil;
  Project.add_writer ~ver "bil.adt"
    ~desc:"print BIL instructions in ADT format" pp_disasm_bil_adt;
  Project.add_writer ~ver "bil.sexp"
    ~desc:"print BIL instructions in Sexp format" pp_disasm_bil_sexp


let () =
  let open Adt in
  let desc = "Abstract Data Type pretty printing format" in
  let ver = Program.version and name = "adt" in
  let create pp = Data.Write.create ~pp () in
  create pp_program |> Program.add_writer  ~desc ~ver name;
  create pp_sub |> Sub.add_writer ~desc ~ver name;
  create pp_arg |> Arg.add_writer ~desc ~ver name;
  create pp_blk |> Blk.add_writer ~desc ~ver name;
  create pp_phi |> Phi.add_writer ~desc ~ver name;
  create pp_def |> Def.add_writer ~desc ~ver name;
  create pp_jmp |> Jmp.add_writer ~desc ~ver name

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Setup various output formats for project data.";
      `S "SEE ALSO";
      `P
        "$(b,bap-plugin-phoenix)(1), $(b,bap-plugin-piqi-printers)(1),
         $(b,bap-plugin-dump-symbols)(1)"

    ] in
  let demangle : string option Config.param =
    let doc = "Demangle symbols, using the specified demangler" in
    Config.(param (some string) "demangled-with" ~doc) in
  let bir_attr : string list Config.param =
    let doc = "When printing IR emit an attribute $(docv)" in
    Config.(param_all string "bir-attr" ~docv:"NAME" ~doc) in
  let ansi_colors : bool Config.param =
    let doc =
      "Allow coloring output with ansi color escape sequences" in
    let default = Unix.isatty Unix.stdout in
    Config.(param bool ~default "with-colors" ~doc) in
  let print_symbols : _ list Config.param =
    let opts = [
      "name", `with_name;
      "addr", `with_addr;
      "size", `with_size;
    ] in
    let doc = sprintf
        "Print found symbols. Optional value \
         defines output format, and can be %s. You can \
         specify this parameter several times, if you \
         want both, for example."
      @@ Config.doc_enum opts in
    let default = [`with_name] in
    Config.(param_all (enum opts) ~default "symbol-format" ~doc) in
  let subs : string list Config.param =
    let doc = "Only display information for symbol $(docv)" in
    Config.(param_all string "symbol" ~docv:"NAME" ~doc) in
  let secs : string list Config.param =
    let doc = "Only display information for section $(docv)" in
    Config.(param_all string "section" ~docv:"NAME" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      main !bir_attr !ansi_colors !demangle !print_symbols !subs !secs)
