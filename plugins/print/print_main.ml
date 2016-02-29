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
  | Some `internal -> fun x -> Demangle.run x
  | Some (`program tool) -> Demangle.run ~tool


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

let print_symbols subs secs demangle fmts ppf proj =
  let demangle = create_demangler demangle in
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


module Cmdline = struct
  open Cmdliner

  let demangle : 'a option Term.t =
    let doc = "Demangle C++ symbols, using either internal \
               algorithm or a specified external tool, e.g. \
               c++filt." in
    let parse = function
      | "internal" -> `Ok `internal
      | name -> `Ok (`program name) in
    let printer fmt = function
      | `internal -> pp_print_string fmt "internal"
      | `program name -> pp_print_string fmt name in
    let spec = parse, printer in
    Arg.(value & opt ~vopt:(Some `internal) (some spec) None &
         info ["demangled"] ~doc)

  let bir_attr : string list Term.t =
    let doc = "When printing IR emit an attribute $(docv)" in
    Arg.(value & opt_all string [] &
         info ["bir-attr"] ~doc ~docv:"NAME")

  let ansi_colors : bool Term.t =
    let doc =
      "Allow coloring output with ansi color escape sequences" in
    let default = Unix.isatty Unix.stdout in
    Arg.(value & opt bool default & info ["with-colors"] ~doc)

  let print_symbols : _ list Term.t =
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
      @@ Arg.doc_alts_enum opts in
    Arg.(value & opt_all (enum opts) [`with_name] &
         info ["symbol-format"] ~doc)

  let subs : string list Term.t =
    let doc = "Only display information for symbol $(docv)" in
    Arg.(value & opt_all string [] & info ["symbol"] ~doc ~docv:"NAME")

  let secs : string list Term.t =
    let doc = "Only display information for section $(docv)" in
    Arg.(value & opt_all string [] & info ["section"] ~doc ~docv:"NAME")

  let man = [
    `S "DESCRIPTION";
    `P "Setup various output formats for project data"
  ]

  let info = Term.info ~doc ~version name

  let main = Term.(const main
                   $bir_attr
                   $ansi_colors
                   $demangle
                   $print_symbols
                   $subs
                   $secs)

  let parse () = Term.eval ~argv ~catch:false (main,info)
end

let () =
  match Cmdline.parse () with
  | `Ok () -> ()
  | `Version | `Help -> exit 0
  | `Error _ -> exit 1
