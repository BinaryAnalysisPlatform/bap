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
  | xs -> List.mem xs


let find_section_for_addr memory addr =
  Memmap.lookup memory addr |> Seq.find_map ~f:(fun (mem,v) ->
      Value.get Image.section v)

let bir memory sub =
  Term.get_attr sub subroutine_addr >>=
  find_section_for_addr memory

let sym memory (name,entry,cfg) =
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
  Seq.filter ~f:(fun ((name,entry,cfg) as fn) ->
      should_print subs name &&
      should_print secs (sec_name (Project.memory proj) sym fn)) |>
  Seq.iter ~f:(fun ((name,entry,cfg) as fn) ->
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

let print_callgraph subs secs ppf proj =
  let prog = extract_program subs secs proj in
  fprintf ppf "%a@."
    Graphs.Callgraph.pp (Program.to_graph prog)

let print_bir_graph subs secs ppf proj =
  let prog = extract_program subs secs proj in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      fprintf ppf "%a@." Graphs.Ir.pp (Sub.to_cfg sub))

let pp_addr ppf addr =
  let width = Addr.bitwidth addr / 4 in
  fprintf ppf "%0*Lx" width
    (Word.(to_int64 addr) |> ok_exn)

let setup_tabs ppf =
  pp_print_as ppf 50 "";
  pp_set_tab ppf ()

let print_disasm pp_insn subs secs ppf proj =
  let memory = Project.memory proj in
  let syms = Project.symbols proj in
  pp_open_tbox ppf ();
  setup_tabs ppf;
  Memmap.filter_map memory ~f:(Value.get Image.section) |>
  Memmap.to_sequence |> Seq.iter ~f:(fun (mem,sec) ->
      Symtab.intersecting syms mem |>
      List.filter ~f:(fun (name,entry,cfg) ->
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
  pp_close_tbox ppf ()

let pp_bil fmt ppf (mem,insn) =
  let pp_bil ppf = Bil.Io.print ~fmt ppf in
  let addr = Memory.min_addr mem in
  fprintf ppf "%a: %s@\n%a@\n" pp_addr addr (Insn.asm insn)
    pp_bil (Insn.bil insn)

let pp_insn fmt ppf (mem,insn) =
  Memory.pp ppf mem;
  pp_print_tab ppf ();
  Insn.Io.print ~fmt ppf insn;
  fprintf ppf "@\n"

let main attrs ansi_colors demangle symbol_fmts subs secs =
  let ver = version in
  let pp_syms =
    Data.Write.create ~pp:(print_symbols subs secs demangle symbol_fmts) () in
  Project.add_writer
    ~desc:"print symbol table" ~ver "symbols" pp_syms;
  let pp_bir = Data.Write.create ~pp:(print_bir subs secs) () in
  List.iter attrs ~f:Text_tags.Attr.show;
  Text_tags.Attr.print_colors ansi_colors;
  Project.add_writer
    ~desc:"print program in IR" ~ver "bir" pp_bir;
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
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Setup various output formats for project data"
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
