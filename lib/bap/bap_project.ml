open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap_future.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Or_error.Monad_infix
open Format

module Event = Bap_event
include Bap_self.Create()

let find name = FileUtil.which name

module State = struct
  type t = {
    tids : Tid.Tid_generator.t;
    name : Tid.Name_resolver.t;
    vars : Var.Id.t;
  }
end

type state = State.t

type t = {
  arch    : arch;
  disasm  : disasm;
  memory  : value memmap;
  storage : dict;
  program : program term;
  symbols : Symtab.t;
  state   : state;
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
    file : string;
    finish : t -> t;
  }

  type t = unit -> result

  let create ?(finish=ident)arch file ~code ~data () = {
    arch; file; code; data; finish;
  }

  let loaders = String.Table.create ()
  let register_loader name loader =
    Hashtbl.set loaders ~key:name ~data:loader

  let is_code v =
    Value.get Image.segment v |>
    Option.value_map ~default:false ~f:Image.Segment.is_executable

  let filter_code mem = Memmap.filter mem ~f:is_code

  let of_image finish file img = {
    arch = Image.arch img;
    data = Image.memory img;
    code = filter_code (Image.memory img);
    file;
    finish;
  }

  let of_image ?loader filename =
    Image.create ?backend:loader filename >>| fun (img,warns) ->
    List.iter warns ~f:(fun e -> warning "%a" Error.pp e);
    let spec = Image.spec img in
    Signal.send Info.got_img img;
    Signal.send Info.got_spec spec;
    let finish proj = {
      proj with
      storage = Dict.set proj.storage Image.specification spec;
      program = Term.map sub_t proj.program ~f:(fun sub ->
          match Term.get_attr sub address with
          | Some a when Addr.equal a (Image.entry_point img) ->
            Term.set_attr sub Sub.entry_point ()
          | _ -> sub)
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
    let data = Memmap.add Memmap.empty mem section in
    {arch; data; code = data; file = filename; finish = ident;}

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


let fresh_state () = State.{
    tids = Tid.Tid_generator.fresh ();
    name = Tid.Name_resolver.fresh ();
    vars = Var.Id.fresh ();
  }

module MVar = struct
  type 'a t = {
    mutable value   : 'a Or_error.t;
    mutable updated : bool;
    compare : 'a -> 'a -> int;
  }

  let create ?(compare=fun _ _ -> 1) x =
    {value=Ok x; updated=true; compare}
  let peek x = ok_exn x.value
  let read x = x.updated <- false; peek x
  let is_updated x = x.updated
  let write x v =
    if x.compare (ok_exn x.value) v <> 0 then x.updated <- true;
    x.value <- Ok v

  let fail x err =
    x.value <- Error err;
    x.updated <- true

  let ignore x =
    Result.iter_error x.value ~f:Error.raise;
    x.updated <- false

  let from_source s =
    let x = create None in
    Stream.observe s (function
        | Ok v -> write x (Some v)
        | Error e -> fail x e);
    x

  let from_optional_source = function
    | None -> create None
    | Some s -> from_source s
end

let phase_triggered phase mvar =
  let trigger = MVar.is_updated mvar in
  if trigger then Signal.send phase (MVar.read mvar);
  trigger

module Cfg = Graphs.Cfg

let empty_disasm = Disasm.create Cfg.empty

let pp_mem ppf mem =
  fprintf ppf "%s"
    (Addr.string_of_value (Memory.min_addr mem))

let pp_disasm_error ppf = function
  | `Failed_to_disasm mem ->
    fprintf ppf "can't disassemble insnt at address %a" pp_mem mem
  | `Failed_to_lift (mem,insn,err) ->
    fprintf ppf "<%s>: %a"
      (Disasm_expert.Basic.Insn.asm insn) Error.pp err

let union_memory m1 m2 =
  Memmap.to_sequence m2 |> Seq.fold ~init:m1 ~f:(fun m1 (mem,v) ->
      Memmap.add m1 mem v)

let symbolize_synthetic prog insns spec =
  if MVar.is_updated spec then
    match MVar.read spec with
    | None -> prog
    | Some spec ->
      let p =
        Bap_synthetic_symbolizer.resolve spec insns prog in
      Signal.send Info.got_program p;
      p
  else prog

let create_exn
    ?disassembler:backend
    ?brancher
    ?symbolizer
    ?rooter
    ?reconstructor
    (read : input)  =
  let state = fresh_state () in
  let mrooter = MVar.from_optional_source rooter in
  let mbrancher = MVar.from_optional_source brancher in
  let msymbolizer = MVar.from_optional_source symbolizer in
  let mreconstructor = MVar.from_optional_source reconstructor in
  let cfg     = MVar.create ~compare:Cfg.compare Cfg.empty in
  let symtab  = MVar.create ~compare:Symtab.compare Symtab.empty in
  let program = MVar.create ~compare:Program.compare (Program.create ()) in
  let spec = MVar.from_source (Stream.map ~f:(fun s -> Ok s) Info.spec) in
  let {Input.arch; data; code; file; finish;} = read () in
  Signal.send Info.got_file file;
  Signal.send Info.got_arch arch;
  Signal.send Info.got_data data;
  Signal.send Info.got_code code;
  let rec loop () =
    let updated = MVar.is_updated mbrancher || MVar.is_updated mrooter in
    let brancher = MVar.read mbrancher
    and rooter   = MVar.read mrooter in
    let disassemble () =
      let run mem =
        let dis =
          Disasm.With_exn.of_mem ?backend ?brancher ?rooter arch mem in
        Disasm.errors dis |>
        List.iter ~f:(fun e -> warning "%a" pp_disasm_error e);
        Disasm.cfg dis in
      Memmap.to_sequence code |>
      Seq.fold ~init:Cfg.empty ~f:(fun cfg (mem,_) ->
          Graphlib.union (module Cfg) cfg (run mem)) |>
      MVar.write cfg  in
    if updated then disassemble ();
    let is_cfg_updated = phase_triggered Info.got_cfg cfg in
    let g = MVar.read cfg in
    let reconstruct () =
      if is_cfg_updated || MVar.is_updated msymbolizer then
        let symbolizer = match MVar.read msymbolizer with
          | None -> Symbolizer.empty
          | Some s -> s in
        let name = Symbolizer.resolve symbolizer in
        let syms =
          Reconstructor.(run (default name (roots rooter)) g) in
        MVar.write symtab syms in
    if is_cfg_updated || MVar.is_updated mreconstructor
    then match MVar.read mreconstructor with
      | Some r ->
        MVar.ignore msymbolizer;
        MVar.write symtab (Reconstructor.run r g)
      | None -> reconstruct ()
    else reconstruct ();
    let is_symtab_updated = phase_triggered Info.got_symtab symtab in
    if is_symtab_updated
    then MVar.write program (Program.lift (MVar.read symtab));
    let _ = phase_triggered Info.got_program program in
    if MVar.is_updated mrooter ||
       MVar.is_updated mbrancher ||
       MVar.is_updated msymbolizer ||
       MVar.is_updated mreconstructor then loop ()
    else
      let disasm = Disasm.create g in
      let program = MVar.read program in
      let program =
        symbolize_synthetic program (Disasm.insns disasm) spec in
      finish {
        disasm;
        program;
        symbols = MVar.read symtab;
        arch; memory=union_memory code data;
        storage = Dict.set Dict.empty filename file;
        state; passes=[]
      } in
  loop ()

let create
    ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor input =
  Or_error.try_with (fun () ->
      create_exn
        ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor input)

let restore_state {state={State.tids; name}} =
  Tid.Tid_generator.store tids;
  Tid.Name_resolver.store name

let with_memory = Field.fset Fields.memory
let with_symbols = Field.fset Fields.symbols
let with_program = Field.fset Fields.program

let with_storage = Field.fset Fields.storage

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
    Symtab.owners project.symbols (Memory.min_addr mem) |>
    List.hd |>
    Option.map ~f:(fun (name,entry,_) ->
        Block.memory entry, name) in
  let find_block mem =
    Symtab.dominators project.symbols mem |>
    List.find_map ~f:(fun (_,_,cfg) ->
        Seq.find_map (Cfg.nodes cfg) ~f:(fun block ->
            if Addr.(Block.addr block = Memory.min_addr mem)
            then Some (Block.memory block, block)
            else None)) in
  let subst_section (mem,name) = function
    | #bound as b -> addr b mem
    | `name -> name in
  let subst_block (mem,block) = function
    | #bound as b -> addr b mem
    | `name -> "blk_"^addr `min mem in
  let asm insn = Insn.asm insn in
  let bil insn = asprintf "%a" Bil.pp (Insn.bil insn) in
  let subst_disasm mem out =
    let inj = match out with `asm -> asm | `bil -> bil in
    match Disasm.of_mem project.arch mem with
    | Error er -> "<failed to disassemble memory region>"
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
    DList.find passes ~f:(fun p -> p.name = name)

  exception Failed of error [@@deriving sexp]

  let fail = function
    | Unsat_dep _ as err -> raise (Failed err)
    | Runtime_error (pass,exn) ->
      let backtrace = Caml.Printexc.get_backtrace () in
      raise (Failed (Runtime_error (pass, Exn.Reraised (backtrace, exn))))

  let is_evaled pass proj =
    List.exists proj.passes ~f:(fun name -> name = pass.name)

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

let () =
  let stream f = Stream.map Info.img ~f:(fun img -> Ok (f img)) in
  let rooter = stream Rooter.of_image in
  let symbolizer = stream Symbolizer.of_image in
  let brancher = stream Brancher.of_image in
  Rooter.Factory.register "internal" rooter;
  Symbolizer.Factory.register "internal" symbolizer;
  Brancher.Factory.register "internal" brancher

include Data.Make(struct
    type nonrec t = t
    let version = "1.0.0"
  end)
