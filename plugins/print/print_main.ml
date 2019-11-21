open Bap_core_theory
open Core_kernel
open Regular.Std
open Graphlib.Std
open Bap.Std
open Format
open Bap_demangle.Std

open Option.Monad_infix

include Self()

type property = {
  extract : project -> addr -> string option;
  explain : string;
}

let find_in_memory extract element proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.find_map ~f:(fun (_,v) ->
      Value.get element v >>| extract)

let properties = [
  "symbol", {
    extract = find_in_memory ident Image.symbol;
    explain = "name of the enclosing symbol, where the symbol is \
               looked up in the file symbol table or debuging \
               information, if any";
  };
  "section", {
    extract = find_in_memory ident Image.section;
    explain = "name of the enclosing section of the file";
  };
  "segment", {
    extract = find_in_memory Image.Segment.name Image.segment;
    explain = "name of the enclosing segment of the file";
  };

  "subroutine", {
    extract = begin fun proj addr ->
      Symtab.find_by_start (Project.symbols proj) addr >>|
      fun (name,_,_) -> name;
    end;
    explain = "name of the enclosing subroutine"
  }
]

let properties_description =
  String.concat ~sep:"; "@@
  List.map properties ~f:(fun (name,{explain}) ->
      sprintf "$(b,%s) - %s\n" name explain)


let matches patterns proj addr =
  List.for_all patterns ~f:(fun ({extract},pattern) ->
      match addr with
      | None -> false
      | Some addr -> match extract proj addr with
        | None -> false
        | Some value -> Re.execp pattern value)


let print_spec ppf proj =
  match Project.get proj Image.specification with
  | None -> warning "The file specification is not found"
  | Some spec ->  Format.fprintf ppf "%a" Ogre.Doc.pp spec

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

let print_symbols patterns demangler fmts ppf proj =
  let demangle = create_demangler demangler in
  let symtab = Project.symbols proj in
  Symtab.to_sequence symtab |>
  Seq.filter ~f:(fun (_,entry,_) ->
      matches patterns proj (Some (Block.addr entry))) |>
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

let extract_program patterns proj =
  Project.program proj |>
  Term.filter sub_t ~f:(fun sub ->
      matches patterns proj (Term.get_attr sub address))

let print_bir patterns sema ppf proj =
  let pp = match sema with
    | None -> Program.pp
    | Some cs -> Program.pp_slots cs in
  Text_tags.with_mode ppf "attr" ~f:(fun () ->
      pp ppf (extract_program patterns proj))

module Adt = struct
  let pr ch = Format.fprintf ch

  let pp_var ppf = Var.Io.print ~fmt:"adt" ppf
  let pp_exp ppf = Exp.Io.print ~fmt:"adt" ppf

  let pp_word ppf = Word.pp_generic ~prefix:`base ~format:`hex ppf

  module Tid = struct
    let pp ppf tid = pr ppf "Tid(%#Ld, %S)"
        (Int63.to_int64 (KB.Object.id tid))
        (Tid.name tid)
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
      fprintf ppf {|Section(%S, %a, "|}
        name pp_word (Memory.min_addr mem);
      Memory.iter ~word_size:`r8 mem ~f:(fun byte ->
          fprintf ppf "%a" pp_byte byte);
      fprintf ppf {|")|} in
    fprintf ppf "Sections(%a)" (pp_seq pp_section) sections

  let pp_memmap ppf memmap =
    let pp_region ppf mem =
      pr ppf "Region(%a,%a)"
        pp_word (Memory.min_addr mem)
        pp_word (Memory.max_addr mem) in
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

let print_callgraph patterns ppf proj =
  let prog = extract_program patterns proj in
  fprintf ppf "%a@."
    Graphs.Callgraph.pp (Program.to_graph prog)

module Ir_pp = struct
  module Cfg = Graphs.Ir

  let succ_tid_of_jmp jmp : tid option = match Jmp.kind jmp with
    | Goto (Direct tid) -> Some tid
    | Int (_,tid) -> Some tid
    | Call t -> Option.(Call.return t >>= function
      | Direct tid -> Some tid
      | _ -> None)
    | _ -> None

  let node_label blk =
    let phis =
      Term.enum phi_t blk |> Seq.map ~f:Phi.to_string in
    let defs =
      Term.enum def_t blk |> Seq.map ~f:Def.to_string in
    let jmps =
      Term.enum jmp_t blk |> Seq.filter_map ~f:(fun jmp ->
          match Jmp.kind jmp with
          | Call _ | Ret _ | Int (_,_) -> Some (Jmp.to_string jmp)
          | Goto _ -> match succ_tid_of_jmp jmp with
            | None -> Some (Jmp.to_string jmp)
            | Some _ -> None) in
    let lines =
      List.concat @@ List.map [phis; defs; jmps] ~f:Seq.to_list in
    let body = String.concat lines |> String.concat_map
                 ~f:(function '\n' -> "\\l"
                            | c -> Char.to_string c) in
    sprintf "\\%s\n%s" (Term.name blk) body

  let string_of_node b =
    sprintf "\\%s" (Tid.to_string (Term.tid b))
end

let pp_bir_cfg ~labeled name ppf g =
  let module Cfg = Graphs.Ir in
  let open Ir_pp in
  let nodes = Cfg.nodes g |> Seq.map ~f:Cfg.Node.label in
  let edges = Cfg.edges g in
  let edge_label e = match Cfg.Edge.cond e g with
    | Bil.Int w when Bitvector.is_one w -> ""
    | exp -> Exp.to_string exp in
  let nodes_of_edge e =
    Cfg.(Node.label (Edge.src e), Node.label (Edge.dst e)) in
  let node_label, edge_label =
    if labeled then Some node_label, Some edge_label
    else None,None in
  Graphlib.Dot.pp_graph
    ~name
    ~subgraph:true
    ~cluster:true
    ~string_of_node ?node_label ?edge_label
    ~nodes_of_edge ~nodes ~edges ppf


let print_bir_graph ~labeled patterns ppf proj =
  let prog = extract_program patterns proj in
  let entries =
    Term.enum sub_t prog |>
    Seq.fold ~init:(Map.empty (module Tid)) ~f:(fun entries sub ->
        match Term.first blk_t sub with
        | None -> entries
        | Some entry ->
          Map.add_exn entries (Term.tid sub) (Term.tid entry)) in
  fprintf ppf "digraph {@\nnode[shape=box];@\n";
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      let name = Sub.name sub in
      fprintf ppf "%a@." (pp_bir_cfg ~labeled name) (Sub.to_cfg sub);
      Term.enum blk_t sub |>
      Seq.iter ~f:(fun src ->
          Term.enum jmp_t src |>
          Seq.iter ~f:(fun jmp -> match Jmp.alt jmp with
              | None -> ()
              | Some dst -> match Jmp.resolve dst with
                | Either.Second _ -> ()
                | Either.First dst ->
                  match Map.find entries dst with
                  | None -> ()
                  | Some dst ->
                    fprintf ppf "\"\\%a\" -> \"\\%a\";@\n"
                      Tid.pp (Term.tid src) Tid.pp dst)));
  fprintf ppf "}"

let pp_addr ppf a =
  Addr.pp_generic ~prefix:`none ~case:`lower ppf a

let setup_tabs ppf =
  pp_print_as ppf 50 "";
  pp_set_tab ppf ()

let sorted_blocks nodes =
  let init = Set.empty (module Block) in
  Seq.fold nodes ~init ~f:Set.add |>
  Set.to_sequence

let sort_fns fns =
  let fns = Array.of_list_rev fns in
  Array.sort fns ~compare:(fun (_,b1,_) (_,b2,_) ->
      Block.compare b1 b2);
  Seq.of_array fns

let print_disasm pp_insn patterns ppf proj =
  let memory = Project.memory proj in
  let syms = Project.symbols proj in
  pp_open_tbox ppf ();
  setup_tabs ppf;
  Memmap.filter_map memory ~f:(Value.get Image.section) |>
  Memmap.to_sequence |> Seq.iter ~f:(fun (mem,sec) ->
      Symtab.intersecting syms mem |>
      List.filter ~f:(fun (_,entry,_) ->
          matches patterns proj (Some (Block.addr entry))) |> function
      | [] -> ()
      | fns ->
        fprintf ppf "@\nDisassembly of section %s@\n" sec;
        Seq.iter (sort_fns fns) ~f:(fun (name,entry,cfg) ->
            fprintf ppf "@\n%a: <%s>@\n" pp_addr (Block.addr entry) name;
            sorted_blocks (Graphs.Cfg.nodes cfg) |> Seq.iter ~f:(fun blk ->
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
  pp_print_tab ppf ()  [@ocaml.warning "-3"];
  Insn.Io.print ~fmt ppf insn;
  fprintf ppf "@\n"

let pp_knowledge ppf _ =
  KB.pp_state ppf @@
  Toplevel.current ()

let compile_patterns subs secs patterns =
  let subs = List.map subs ~f:(sprintf "symbol:%s") in
  let secs = List.map secs ~f:(sprintf "section:%s") in
  List.concat_no_order [
    subs; secs; patterns
  ] |> List.map ~f:(fun pattern ->
      match String.index pattern ':' with
      | None ->
        invalid_argf "wrong pattern %S, expected `:`, none found"
          pattern ()
      | Some pos ->
        let property = String.sub ~pos:0 ~len:pos pattern in
        let pattern = String.subo ~pos:(pos+1) pattern in
        let pattern = Re.Pcre.re pattern |> Re.compile in
        match List.Assoc.find properties property ~equal:String.equal
        with None ->
          invalid_argf "unknown property %S" property ()
           | Some property -> property,pattern)

let main attrs ansi_colors demangle symbol_fmts subs secs patterns doms =
  let patterns = compile_patterns subs secs patterns in
  let ver = version in
  let pp_syms =
    Data.Write.create ~pp:(print_symbols patterns demangle symbol_fmts) () in
  Project.add_writer
    ~desc:"print symbol table" ~ver "symbols" pp_syms;
  let pp_bir = Data.Write.create ~pp:(print_bir patterns doms) () in
  let pp_adt = Data.Write.create ~pp:Adt.pp_project () in

  List.iter attrs ~f:Text_tags.Attr.show;
  Text_tags.Attr.print_colors ansi_colors;
  Project.add_writer
    ~desc:"print program in IR" ~ver "bir" pp_bir;
  Project.add_writer
    ~desc:"print program IR in ADT format" ~ver "adt" pp_adt;
  let pp_callgraph =
    Data.Write.create ~pp:(print_callgraph patterns) () in
  Project.add_writer ~ver "callgraph"
    ~desc:"print program callgraph in DOT format" pp_callgraph;
  let pp_cfg = Data.Write.create ~pp:(print_bir_graph ~labeled:true patterns) () in
  let pp_calls = Data.Write.create ~pp:(print_bir_graph ~labeled:false patterns) () in
  let pp_spec = Data.Write.create ~pp:print_spec () in
  let pp_disasm_bil =
    Data.Write.create ~pp:(print_disasm (pp_bil "pretty") patterns) () in
  let pp_disasm_bil_adt =
    Data.Write.create ~pp:(print_disasm (pp_bil "adt") patterns) () in
  let pp_disasm_bil_sexp =
    Data.Write.create ~pp:(print_disasm (pp_bil "sexp") patterns) () in
  let pp_disasm_asm =
    Data.Write.create ~pp:(print_disasm (pp_insn "asm") patterns) () in
  let pp_disasm_adt =
    Data.Write.create ~pp:(print_disasm (pp_insn "adt") patterns) () in
  let pp_disasm_decoded =
    Data.Write.create ~pp:(print_disasm (pp_insn "pretty") patterns) () in
  let pp_disasm_sexp =
    Data.Write.create ~pp:(print_disasm (pp_insn "sexp") patterns) () in

  let pp_knowledge = Data.Write.create ~pp:(pp_knowledge) () in

  Project.add_writer ~ver "knowledge"
    ~desc:"dumps the knowledge base" pp_knowledge;
  Project.add_writer ~ver "cfg"
    ~desc:"print rich CFG for each procedure" pp_cfg;
  Project.add_writer ~ver "graph"
    ~desc:"print unlabeled CFG for each procedure" pp_calls;
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
    ~desc:"print BIL instructions in Sexp format" pp_disasm_bil_sexp;
  Project.add_writer
    ~desc:"print the file specification in the OGRE format" ~ver "ogre" pp_spec

let () =
  Config.when_ready @@ fun _ ->
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
      `P "Provides various formats for dumping the project data structure.";
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
    let doc = "same as $(b,--print-matching=symbol:)$(docv)" in
    Config.(param_all string "symbol" ~docv:"NAME" ~doc) in
  let secs : string list Config.param =
    let doc = "same as $(b,--print-matching=section:)$(docv)" in
    Config.(param_all string "section" ~docv:"NAME" ~doc) in

  let patterns : string list Config.param =
    let doc =
      sprintf
        "Only print elements that matches with the provided patterns.
      A pattern consists of the name of a property and a regular
      expression, which denotes a set of values of this property.
      The property name and the regular expression are separated wit
      the $(b,:) symbol, e.g., $(b,symbol:main) will print all
      elements that belong to the symbol entry $(b,main).
      The syntax of the regular expressions is PCRE with partial
      matching. The following properties are supported: %s."
        properties_description in
    Config.(param_all string "matching" ~doc) in

  let semantics : string list option Config.param =
    let doc =
      "Display the $(docv) semantics of the program. If used without
       an argument then all semantic values associated with terms will
       be printed. Otherwise only the selected (if present) will be
       printed." in
    Config.(param (some (list string)) ~as_flag:(Some [])
              ~doc ~docv:"SEMANTICS-LIST" "semantics") in
  Config.when_ready (fun {Config.get=(!)} ->
      main !bir_attr !ansi_colors !demangle !print_symbols !subs !secs
        !patterns
        !semantics)
