open Core_kernel
open Regular.Std
open Bap_core_theory
open Graphlib.Std
open Monads.Std
open Bap_future.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Or_error.Monad_infix
open Format

module Driver = Bap_disasm_driver

module Event = Bap_main_event
module Buffer = Caml.Buffer
include Bap_self.Create()

let query doc attr =
  match Ogre.eval (Ogre.request attr) doc with
  | Error err ->
    invalid_argf "Malformed ogre specification: %s"
      (Error.to_string_hum err) ()
  | Ok bias -> bias

module Spec = struct
  module Fact = Ogre.Make(KB)
  open Fact.Syntax

  let provide slot obj value =
    Fact.lift @@ KB.provide slot obj value

  let str field slot unit =
    Fact.request field >>= function
    | None | Some "" | Some "unknown" ->
      Fact.return ()
    | value -> provide slot unit value

  let bool field slot unit =
    Fact.request field >>= provide slot unit

  let int field slot unit =
    Fact.request field >>= function
    | Some x ->
      provide slot unit (Some (Int64.to_int_exn x))
    | None -> Fact.return ()

  let facts fields unit =
    Fact.List.iter fields ~f:(fun provide -> provide unit)

  let init_unit =
    let open Theory.Unit in
    let open Image.Scheme in
    facts [
      str arch Target.arch;
      str subarch Target.subarch;
      str vendor Target.vendor;
      str system Target.system;
      str abi Target.abi;
      int bits Target.bits;
      bool is_little_endian Target.is_little_endian;
    ]

  let provide spec unit =
    let open KB.Syntax in
    Fact.exec (init_unit unit) spec >>= function
    | Error err ->
      invalid_argf "Failed to provide the image specification \
                    to the knowledge base: %s"
        (Error.to_string_hum err) ()
    | Ok _doc -> KB.return ()


  let init arch =
    let module Field = Image.Scheme in
    let open Ogre.Syntax in
    let bits = Int64.of_int (Size.in_bits (Arch.addr_size arch))  in
    let statements = Ogre.all [
        Ogre.provide Field.arch (Arch.to_string arch);
        Ogre.provide Field.bits bits;
        Ogre.provide Field.format "raw";
        Ogre.provide Field.is_little_endian @@
        match Arch.endian arch with
        | LittleEndian -> true
        | BigEndian -> false
      ] in
    match Ogre.exec statements Ogre.Doc.empty with
    | Error err ->
      failwithf "got a malformed ogre document: %s"
        (Error.to_string_hum err) ();
    | Ok doc -> doc
end

let with_arch arch mems =
  let open KB.Syntax in
  let width = Size.in_bits (Arch.addr_size arch) in
  KB.promising Arch.slot ~promise:(fun label ->
      KB.collect Theory.Label.addr label >>| function
      | None -> `unknown
      | Some p ->
        let p = Word.create p width in
        if Memmap.contains mems p
        then arch
        else `unknown)

let with_filename spec arch data code path =
  let open KB.Syntax in
  let width = Size.in_bits (Arch.addr_size arch) in
  let bias = Option.map (query spec Image.Scheme.bias)
      ~f:(fun x -> Bitvec.(int64 x mod modulus width)) in
  KB.promising Theory.Label.unit ~promise:(fun label ->
      KB.collect Theory.Label.addr label >>=? fun addr ->
      let addr = Word.create addr width in
      if Memmap.contains data addr || Memmap.contains code addr
      then
        Theory.Unit.for_file path >>= fun unit ->
        Spec.provide spec unit >>= fun () ->
        KB.provide Theory.Unit.bias unit bias >>= fun () ->
        KB.provide Theory.Unit.path unit (Some path) >>| fun () ->
        Some unit
      else KB.return None)


module State = struct
  open KB.Syntax
  module Dis = Bap_disasm_driver
  module Sub = Bap_disasm_calls
  module Rec = Disasm_expert.Recursive

  type t = {
    disassembly : Dis.state;
    subroutines : Sub.t;
  } [@@deriving bin_io]

  let empty = {
    disassembly = Dis.init;
    subroutines = Sub.empty;
  }

  let equal x y =
    Dis.equal x.disassembly y.disassembly &&
    Sub.equal x.subroutines y.subroutines


  let disassemble self mem =
    Dis.scan mem self.disassembly >>| fun disassembly ->
    {self with disassembly}

  let partition self =
    Sub.update self.subroutines self.disassembly >>| fun subroutines ->
    {self with subroutines}

  let symbols {disassembly; subroutines} =
    Symtab.create disassembly subroutines

  let cfg {disassembly} =
    Disasm_expert.Recursive.global_cfg disassembly

  let disassembly {disassembly=d} = d
  let subroutines {subroutines=s} = s

  let set_length set =
    Sexp.Atom (string_of_int @@ Set.length set)

  let inspect {disassembly; subroutines} = Sexp.List [
      List [
        Atom ":number-of-basic-blocks";
        set_length (Dis.blocks disassembly);
      ];
      List [
        Atom ":number-of-subroutines";
        set_length (Sub.entries subroutines);
      ]
    ]

  let slot = KB.Class.property Theory.Unit.cls
      ~package:"bap" "disassembly"
      ~persistent:(KB.Persistent.of_binable(module struct
                     type nonrec t = t [@@deriving bin_io]
                   end)) @@
    KB.Domain.flat ~empty ~equal "disassembly" ~inspect

  module Toplevel = struct
    let run spec arch ~code ~data file k =
      let result = Toplevel.var "disassembly-result" in
      Toplevel.put result begin
        with_arch arch code @@ fun () ->
        with_filename spec arch code data file @@ fun () ->
        k
      end;
      Toplevel.get result
  end
end

type state = State.t [@@deriving bin_io]

type t = {
  arch    : arch;
  spec    : Ogre.doc;
  state   : State.t;
  disasm  : disasm Lazy.t;
  memory  : value memmap;
  storage : dict;
  program : program term Lazy.t;
  symbols : Symtab.t Lazy.t;
  passes  : string list;
} [@@deriving fields]

module Info = struct
  let file,got_file = Stream.create ()
  let arch,got_arch = Stream.create ()
  let data,got_data = Stream.create ()
  let code,got_code = Stream.create ()
  let img, got_img  = Stream.create ()
  let cfg, got_cfg  = Stream.create ()
  let symtab,got_symtab = Stream.create ()
  let program,got_program = Stream.create ()
  let spec,got_spec = Stream.create ()
end

module Input = struct
  type result = {
    arch : arch;
    data : value memmap;
    code : value memmap;
    spec : Ogre.doc;
    file : string;
    finish : t -> t;
  }

  type t = unit -> result

  let create ?(finish=ident) arch file ~code ~data () = {
    arch; file; code; data; finish; spec=Spec.init arch;
  }

  let loaders = String.Table.create ()
  let register_loader name loader =
    Hashtbl.set loaders ~key:name ~data:loader

  let is_code v =
    Value.get Image.code_region v |> Option.is_some

  let is_data v = not (is_code v)

  let of_image finish file img = {
    arch = Image.arch img;
    data = Memmap.filter ~f:is_data (Image.memory img);
    code = Memmap.filter ~f:is_code (Image.memory img);
    spec = Image.spec img;
    file;
    finish;
  }

  let symtab_agent =
    let reliability = KB.Agent.authorative in
    KB.Agent.register "symtab"
      ~reliability
      ~desc:"extracts symbols from symbol tables"
      ~package:"bap"

  let provide_image file image =
    let image_symbols = Symbolizer.(set_path (of_image image) file) in
    let image_roots = Rooter.(set_path (of_image image) file) in
    info "providing rooter and symbolizer from image of %a"
      Sexp.pp_hum ([%sexp_of : string option] (Image.filename image));
    Symbolizer.provide symtab_agent image_symbols;
    Rooter.provide image_roots

  let of_image ?loader filename =
    Image.create ?backend:loader filename >>| fun (img,warns) ->
    List.iter warns ~f:(fun e -> warning "%a" Error.pp e);
    let spec = Image.spec img in
    Signal.send Info.got_img img;
    provide_image filename img;
    let finish proj = {
      proj with
      storage = Dict.set proj.storage Image.specification spec;
      program =
        Lazy.map proj.program ~f:(fun prog ->
            Term.map sub_t prog ~f:(fun sub ->
                match Term.get_attr sub address with
                | Some a when Addr.equal a (Image.entry_point img) ->
                  Term.set_attr sub Sub.entry_point ()
                | _ -> sub))
    } in
    of_image finish filename img

  let from_image ?loader filename () =
    of_image ?loader filename |> ok_exn

  let file ?loader ~filename = match loader with
    | None -> from_image filename
    | Some name -> match Hashtbl.find loaders name with
      | None -> from_image ?loader filename
      | Some load -> load filename

  let null arch : addr =
    Addr.of_int 0 ~width:(Arch.addr_size arch |> Size.in_bits)

  let binary ?base arch ~filename () =
    let big = Bap_fileutils.readfile filename in
    if Bigstring.length big = 0 then invalid_arg "file is empty";
    let base = Option.value base ~default:(null arch) in
    let mem = Memory.create (Arch.endian arch) base big |> ok_exn in
    let section = Value.create Image.section "bap.user" in
    let code = Memmap.add Memmap.empty mem section in
    let data = Memmap.empty  in
    let spec = Spec.init arch in
    {arch; data; code; file = filename; finish = ident; spec}

  let available_loaders () =
    Hashtbl.keys loaders @ Image.available_backends ()
end

type input = Input.t
type project = t

type bound = [`min | `max] [@@deriving sexp]
type spec = [`name | bound] [@@deriving sexp]

type subst = [
  | `section of spec
  | `symbol of spec
  | `memory of bound
  | `block of bound
  | `asm
  | `bil
] [@@deriving sexp]


let roots rooter = match rooter with
  | None -> []
  | Some r -> Rooter.roots r |> Seq.to_list

module Cfg = Graphs.Cfg

let empty_disasm = Disasm.create Cfg.empty

let pp_mem ppf mem =
  fprintf ppf "%s"
    (Addr.string_of_value (Memory.min_addr mem))

let pp_disasm_error ppf = function
  | `Failed_to_disasm mem ->
    fprintf ppf "can't disassemble an instruction at address %a" pp_mem mem
  | `Failed_to_lift (_mem,insn,err) ->
    fprintf ppf "<%s>: %a"
      (Disasm_expert.Basic.Insn.asm insn) Error.pp err

let union_memory m1 m2 =
  Memmap.to_sequence m2 |> Seq.fold ~init:m1 ~f:(fun m1 (mem,v) ->
      Memmap.add m1 mem v)

let set_package package = match package with
  | None -> KB.return ()
  | Some pkg -> KB.Symbol.set_package pkg

let state {state} = state

let unused_options =
  List.iter ~f:(Option.iter ~f:(fun name ->
      warning "Project.create parameter %S is deprecated, \
               please consult the documentation for the proper \
               alternative" name))

let (=?) name = Option.map ~f:(fun _ -> name)

let create
    ?package
    ?state
    ?disassembler:p1
    ?brancher:p2
    ?symbolizer:p3
    ?rooter:p4
    ?reconstructor:p5
    (read : input)  =
  try
    unused_options [
      "disassembler" =? p1;
      "brancher" =? p2;
      "symbolizer" =? p3;
      "rooter" =? p4;
      "rooter" =? p5;
    ];
    let {Input.arch; data; code; file; spec; finish} = read () in
    Signal.send Info.got_file file;
    Signal.send Info.got_arch arch;
    Signal.send Info.got_data data;
    Signal.send Info.got_code code;
    Signal.send Info.got_spec spec;
    let run k =
      State.Toplevel.run spec arch ~code ~data file k in
    let state = match state with
      | Some state -> state
      | None ->
        let compute_state =
          let open KB.Syntax in
          set_package package >>= fun () ->
          Theory.Unit.for_file file >>= fun unit ->
          KB.collect State.slot unit >>= fun state ->
          if KB.Domain.is_empty (KB.Slot.domain State.slot) state
          then
            Memmap.to_sequence code |>
            KB.Seq.fold ~init:State.empty ~f:(fun k (mem,_) ->
                State.disassemble k mem) >>=
            State.partition >>= fun state ->
            KB.provide State.slot unit state >>| fun () ->
            state
          else !!state in
        run compute_state in
    let cfg = lazy (run @@ State.cfg state) in
    let symbols = lazy (run @@ State.symbols state) in
    Result.return @@ finish {
      state;
      spec;
      disasm = Lazy.(cfg >>| Disasm.create);
      program = Lazy.(symbols >>| Program.lift) ;
      symbols;
      arch; memory=union_memory code data;
      storage = Dict.set Dict.empty filename file;
      passes=[]
    }
  with
  | Toplevel.Conflict err ->
    let open Error.Internal_repr in
    let msg =
      String (Format.asprintf "Knowledge Base Conflict: %a"
                KB.Conflict.pp err) in
    Error (to_info msg)
  | exn -> Or_error.of_exn ~backtrace:`Get exn

let specification = spec

let symbols {symbols} = Lazy.force symbols
let disasm {disasm} = Lazy.force disasm
let program {program} = Lazy.force program

let with_symbols p x = {p with symbols = lazy x}
let with_program p x = {p with program = lazy x}
let map_program p ~f = {p with program = Lazy.map p.program ~f}

let with_memory = Field.fset Fields.memory
let with_storage = Field.fset Fields.storage

let restore_state _ =
  failwith "Project.restore_state: this function should no be used.
    Please use the Toplevel module to save/restore the state."

let set t tag x =
  with_storage t @@
  Dict.set t.storage tag x

let get t = Dict.find t.storage
let has t = Dict.mem t.storage
let del t tag = with_storage t @@ Dict.remove t.storage tag

let subst_of_string = function
  | "section" | "section_name" -> Some (`section `name)
  | "section_addr" | "section_min_addr" -> Some (`section `min)
  | "section_max_addr" -> Some (`section `max)
  | "symbol" | "symbol_name" -> Some (`symbol `name)
  | "symbol_addr" | "symbol_min_addr" -> Some (`symbol `min)
  | "symbol_max_addr" -> Some (`symbol `max)
  | "bil" -> Some (`bil)
  | "asm" -> Some (`asm)
  | "block" | "block_name" ->  Some (`block `name)
  | "block_addr" | "block_min_addr" -> Some (`block `min)
  | "block_max_addr" -> Some (`block `max)
  | "min_addr" | "addr" -> Some (`memory `min)
  | "max_addr" -> Some (`memory `max)
  | _ -> None

let addr which mem =
  let take = match which with
    |  `min -> Memory.min_addr
    | `max -> Memory.max_addr in
  sprintf "0x%s" @@ Addr.string_of_value (take mem)


let tag_memory project mem tag x =
  {project with
   memory = Memmap.add project.memory mem (Value.create tag x) }

let substitute project mem tag value : t =
  let find_tag tag mem =
    Memmap.dominators project.memory mem |>
    Seq.find_map ~f:(fun (mem,v) -> match Value.get tag v with
        | Some reg -> Some (mem,reg)
        | None -> None) in
  let find_section = find_tag Image.section in
  let find_symbol mem =
    Symtab.owners (symbols project) (Memory.min_addr mem) |>
    List.hd |>
    Option.map ~f:(fun (name,entry,_) ->
        Block.memory entry, name) in
  let find_block mem =
    Symtab.dominators (symbols project) mem |>
    List.find_map ~f:(fun (_,_,cfg) ->
        Seq.find_map (Cfg.nodes cfg) ~f:(fun block ->
            if Addr.(Block.addr block = Memory.min_addr mem)
            then Some (Block.memory block, block)
            else None)) in
  let subst_section (mem,name) = function
    | #bound as b -> addr b mem
    | `name -> name in
  let subst_block (mem,_block) = function
    | #bound as b -> addr b mem
    | `name -> "blk_"^addr `min mem in
  let asm insn = Insn.asm insn in
  let bil insn = asprintf "%a" Bil.pp (Insn.bil insn) in
  let subst_disasm mem out =
    let inj = match out with `asm -> asm | `bil -> bil in
    match Disasm.of_mem project.arch mem with
    | Error _er -> "<failed to disassemble memory region>"
    | Ok dis ->
      Disasm.insns dis |>
      Seq.map ~f:(fun (_,insn) -> inj insn) |> Seq.to_list |>
      String.concat ~sep:"\n" in

  let apply_subst find mem subst spec value =
    match find mem with
    | Some thing -> subst thing spec
    | None -> value in
  let sub mem x =
    let buf = Buffer.create (String.length x) in
    Buffer.add_substitute buf (fun x -> match subst_of_string x with
        | Some (`section spec) ->
          apply_subst find_section mem subst_section spec x
        | Some (`symbol spec) ->
          apply_subst find_symbol mem subst_section spec x
        | Some (`memory bound) -> addr bound mem
        | Some (`block spec) ->
          apply_subst find_block mem subst_block spec x
        | Some (`bil | `asm as out) -> subst_disasm mem out
        | None -> x) x;
    Buffer.contents buf in
  tag_memory project mem tag (sub mem value)

module DList = Doubly_linked

type pass = {
  name : string;
  main : (t -> t) sexp_opaque;
  deps : string sexp_list;
  auto : bool;
  once : bool;
  starts     : float stream sexp_opaque;
  finishes   : float stream sexp_opaque;
  failures   : float stream sexp_opaque;
  successes  : float stream sexp_opaque;
} [@@deriving sexp_of]

let passes : pass DList.t = DList.create ()
let pass_registrations,pass_registered = Stream.create ()

let forget : pass DList.Elt.t -> unit = fun _ -> ()


let name_of_bundle () =
  let module Self = Bap_self.Create() in
  Self.name

let register_pass ?(autorun=false) ?(runonce=autorun) ?(deps=[]) ?name main : unit =
  let pref = name_of_bundle () in
  let name = match name with
    | None -> pref
    | Some name -> pref ^ "-" ^ name in
  let starts,started = Stream.create () in
  let successes,succeded = Stream.create () in
  let failures,failed = Stream.create () in
  let finishes =
    Stream.either successes failures |>
    Stream.map ~f:Either.value in
  let now () = Unix.gettimeofday () in
  let main project =
    Signal.send started (now ());
    try
      let project = main project in
      Signal.send succeded (now ());
      project
    with exn ->
      Signal.send failed (now ());
      raise exn in
  let pass = {
    name; main; deps;
    once = runonce; auto = autorun;
    starts; finishes;
    failures; successes;
  } in
  DList.insert_last passes pass |> forget;
  Signal.send pass_registered pass

let register_pass' ?autorun ?runonce ?deps ?name v : unit =
  register_pass ?autorun ?runonce ?deps ?name (fun p -> v p; p)

type second = float
module Pass = struct
  type t = pass [@@deriving sexp_of]
  type error =
    | Unsat_dep of pass * string
    | Runtime_error of pass * exn
  [@@deriving variants, sexp_of]

  let find name : pass option =
    DList.find passes ~f:(fun p -> String.equal p.name name)

  exception Failed of error [@@deriving sexp]

  let fail = function
    | Unsat_dep _ as err -> raise (Failed err)
    | Runtime_error (pass,exn) ->
      let backtrace = Caml.Printexc.get_backtrace () in
      raise (Failed (Runtime_error (pass, Exn.Reraised (backtrace, exn))))

  let is_evaled pass proj =
    List.exists proj.passes ~f:(fun name -> String.equal name pass.name)

  let eval pass proj = {
    (pass.main proj) with
    passes = pass.name :: proj.passes
  }

  let rec exec proj pass =
    if pass.once && is_evaled pass proj then proj
    else
      let deps = List.map pass.deps ~f:(fun name -> match find name with
          | None -> fail @@ unsat_dep pass name
          | Some dep -> dep) in
      let proj = List.fold deps ~init:proj ~f:exec in
      try eval pass proj with
        exn -> fail @@ runtime_error pass exn

  let run_exn pass proj = exec proj pass

  let run pass proj : (project,error) Result.t =
    try Ok (exec proj pass) with
    | Failed error -> Error error

  let name p = p.name
  let autorun p  = p.auto
  let starts p = p.starts
  let finishes p = p.finishes
  let failures p = p.failures
  let successes p = p.successes
end

let passes () = DList.to_list passes
let find_pass = Pass.find

module Registry(T : T)(I : T) = struct
  open Bap_knowledge
  type info = {
    name : Knowledge.Name.t;
    desc : string option;
    extra : I.t;
  }

  let registry : (Knowledge.name, string option * T.t * I.t) Hashtbl.t =
    Hashtbl.create (module Knowledge.Name)

  let register ?desc ?package name extra entity =
    let name = Knowledge.Name.create ?package name in
    if Hashtbl.mem registry name then
      invalid_argf "An element with name %s is already registered \
                    please choose a unique name"
        (Knowledge.Name.show name) ();
    Hashtbl.add_exn registry name (desc,entity,extra)

  let find ?package name =
    let name = Knowledge.Name.read ?package name in
    match Hashtbl.find registry name with
    | Some (_,x,_) -> Some x
    | None -> None

  let registered () =
    Hashtbl.to_alist registry |>
    List.map ~f:(fun (name,(desc,_,extra)) -> {name; desc; extra})

  let name {name} = name
  let desc = function
    | {desc=None} -> "not provided"
    | {desc=Some txt} -> txt
  let extra {extra} = extra
end

module Collator = struct
  type t = Collator : {
      prepare : project -> 's;
      collate : int -> 's -> project -> 's;
      summary : 's -> unit;
    } -> t

  include Registry(struct type nonrec t = t end)(Unit)

  let apply (Collator {prepare; collate; summary}) projects =
    match Seq.split_n projects 1 with
    | [base],rest ->
      summary @@
      Seq.foldi ~init:(prepare base) rest ~f:collate
    | _ -> ()

  let register ?desc ?package name ~prepare ~collate ~summary =
    register ?desc ?package name () @@ Collator {
      prepare;
      collate;
      summary;
    }
end

module Analysis = struct
  open Bap_knowledge
  open Bap_core_theory
  open Knowledge.Syntax

  type ctxt = {
    rule : string;
    pos : int;
    parsed : string list;
    inputs : string list;
  }

  type 'a arg = {
    parse : ctxt -> ('a * ctxt) knowledge;
    desc : string;
    rule : string;
  }

  type ('a,'r) args = {
    run : 'a -> 'r arg;
    grammar : string list;
  }

  type problem =
    | No_input
    | Bad_syntax of string
    | Trailing_input

  type parse_error = {
    ctxt : ctxt;
    problem : problem
  }

  type Knowledge.conflict += Fail of parse_error


  let fail ctxt problem =
    Knowledge.fail (Fail {ctxt; problem})

  let string_of_problem = function
    | No_input -> "expects an argument"
    | Bad_syntax msg -> msg
    | Trailing_input -> "too many arguments"

  let string_of_parse_error {ctxt; problem} =
    sprintf "Syntax error: when parsing rule %s of argument %d - %s"
      ctxt.rule (ctxt.pos+1) (string_of_problem problem)

  let () = Knowledge.Conflict.register_printer @@ function
    | Fail err -> Some (string_of_parse_error err)
    | _ -> None

  let required parse ctxt =
    match ctxt.inputs with
    | [] -> fail ctxt No_input
    | x :: xs ->
      let fail x = fail ctxt (Bad_syntax x) in
      parse ~fail x >>| fun r -> r,{
          ctxt with pos = ctxt.pos + 1;
                    parsed = x :: ctxt.parsed;
                    inputs = xs;
        }

  let argument ?(desc="No description") ~parse rule = {
    parse=(required parse); rule; desc;
  }

  let optional arg = {
    rule = sprintf "[%s]" arg.rule;
    desc = arg.desc;
    parse = fun ctxt -> match ctxt.inputs with
      | [] -> KB.return (None,ctxt)
      | _ -> arg.parse ctxt >>| fun (x,ctxt) -> Some x,ctxt
  }

  let pull_keyword kw inputs =
    let rec loop searched = function
      | [] -> None
      | [k] when String.equal k kw && List.is_empty searched ->
        Some []
      | k :: x :: xs when String.equal k kw ->
        Some (x :: List.rev_append searched xs)
      | x :: xs -> loop (x::searched) xs in
    loop [] inputs

  let filter_flag kw inputs =
    let rec loop searched = function
      | [] -> None
      | k :: xs when String.equal k kw ->
        Some (List.rev_append searched xs)
      | x :: xs -> loop (x::searched) xs in
    loop [] inputs

  let keyword key arg = {
    rule = sprintf "[:%s %s]" key arg.rule;
    desc = "an argument prefixed by the keyword";
    parse = fun ctxt ->
      match pull_keyword (":"^key) ctxt.inputs with
      | None -> KB.return (None,ctxt)
      | Some inputs -> arg.parse {ctxt with inputs} >>|
        fun (x,ctxt) -> Some x,ctxt
  }

  let flag key = {
    rule = sprintf "[:%s]" key;
    desc = "an optional flag";
    parse = fun ctxt ->
      match filter_flag (":"^key) ctxt.inputs with
      | None -> KB.return (false,ctxt)
      | Some inputs -> KB.return (true, {ctxt with inputs})
  }

  let apply_until_exhausted ctxt arg =
    let rec loop rs ctxt = match ctxt.inputs with
      | [] -> KB.return (List.rev rs,ctxt)
      | _ -> arg.parse ctxt >>= fun (r,ctxt) ->
        loop (r::rs) ctxt in
    loop [] ctxt

  let rest arg = {
    rule = sprintf "[%s] ..." arg.rule;
    desc = arg.desc;
    parse = fun ctxt -> apply_until_exhausted ctxt arg
  }

  let empty = {
    rule = "";
    desc = "no arguments are expected";
    parse = fun ctxt -> match ctxt.inputs with
      | [] -> KB.return ((),ctxt)
      | _ -> fail ctxt Trailing_input
  }


  let parse_string ~fail:_ x = !!x

  let string = argument "<string>"
      ~parse:parse_string
      ~desc:"a sequence of characters without whitespaces"

  let parse_object cls ~fail:_ x = KB.Object.read cls x

  let object_of cls =
    argument "<label>"
      ~parse:(parse_object cls)
      ~desc:(sprintf "an object of the %s" name)

  let program = object_of Theory.Program.cls
  let unit = object_of Theory.Unit.cls

  let parse_bitvec ~fail str =
    try !!(Bitvec.of_string str)
    with Invalid_argument msg ->
      fail msg

  let bitvec = argument "<bitvec>"
      ~parse:parse_bitvec
      ~desc:"a bitvector of arbitrary length"

  module Arg = struct
    type 'a t = 'a arg
    let map arg ~f = {
      arg with
      parse = fun ctxt ->
        arg.parse {ctxt with rule = arg.rule} >>| fun (x,ctxt) ->
        f x,ctxt
    }

    let apply ({parse=f} as lhs) ({parse=x} as rhs) = {
      rule = lhs.rule ^ " " ^ rhs.rule;
      desc = "";
      parse = fun ctxt ->
        f {ctxt with rule = lhs.rule} >>= fun (f,ctxt) ->
        x {ctxt with rule = rhs.rule} >>| fun (x,ctxt) ->
        f x,ctxt
    }
  end

  let ($) t arg = {
    grammar = arg.rule :: t.grammar;
    run = fun f -> Arg.apply (t.run f) arg
  }
  let args a = {
    grammar = [a.rule];
    run = fun f -> Arg.map ~f a
  }
  let apply ~f args = args.run f

  let run inputs args code =
    let ctxt = {rule = ""; pos=0; parsed=[]; inputs} in
    (apply args ~f:code).parse ctxt >>= fun (f,_ctxt) ->
    f

  type t = string list -> unit knowledge
  include Registry(struct type nonrec t = t end)(struct
      type t = string list
    end)

  let register ?desc ?package name args analysis =
    let code inputs = run inputs args analysis in
    register ?desc ?package name (List.rev args.grammar) code

  let apply f xs = f xs

  let grammar = extra

  module Grammar = struct
    type t = string list
    let to_string = String.concat ~sep:" "
  end
  type grammar = Grammar.t
end

module type S = sig
  type t
  val empty : t
  val of_image : image -> t
  module Factory : Bap_disasm_source.Factory with type t = t
end

include Data.Make(struct
    type nonrec t = t
    let version = "2.0.0"
  end)

let () =
  Data.set_module_name instance "Bap.Std.Project"

let () =
  KB.Rule.(declare ~package:"bap" "project-filename" |>
           dynamic ["input"] |>
           dynamic ["data"; "code"; "path"] |>
           require Theory.Label.addr |>
           provide Theory.Label.unit |>
           comment {|
On [Project.create input] provides [path] for the address [x]
if [x] in [data] or [x] in [code].
|});
  KB.Rule.(declare ~package:"bap" "project-arch" |>
           dynamic ["input"] |>
           dynamic ["arch"; "code"] |>
           require Theory.Label.addr |>
           provide Arch.slot |>
           comment {|
On [Project.create input] provides [arch] for the address [x]
if [x] in [code].
|})
