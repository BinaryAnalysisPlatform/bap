open Core_kernel
open Regular.Std
open Bap_core_theory
open Graphlib.Std
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

let find name = FileUtil.which name

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

let with_filename arch data code path =
  let open KB.Syntax in
  let width = Size.in_bits (Arch.addr_size arch) in
  KB.promising Theory.Label.path ~promise:(fun label ->
      KB.collect Theory.Label.addr label >>|? fun addr ->
      let addr = Word.create addr width in
      if Memmap.contains data addr || Memmap.contains code addr
      then Some path
      else None)


module Kernel = struct
  open KB.Syntax
  module Driver = Bap_disasm_driver
  module Calls = Bap_disasm_calls
  module Disasm = Disasm_expert.Recursive

  type t = {
    default : arch;
    package : string option;
    state : Driver.state;
    calls : Calls.t;
  } [@@deriving bin_io]

  let empty ?package arch = {
    default = arch;
    package;
    state = Driver.init;
    calls = Calls.empty;
  }

  let update self mem =
    Driver.scan mem self.state >>= fun state ->
    Calls.update self.calls state >>| fun calls ->
    {self with state; calls}

  let symtab {state; calls} = Symtab.create state calls
  let disasm {state} =
    Disasm_expert.Recursive.global_cfg state

  module Toplevel = struct
    let result = Toplevel.var "result"
    let run arch ~code ~data file k =
      Toplevel.put result begin
        with_arch arch code @@ fun () ->
        with_filename arch code data file @@ fun () ->
        k >>= fun k ->
        disasm k >>= fun g ->
        symtab k >>| fun s -> g,s,k
      end;
      Toplevel.get result
  end
end

type state = Kernel.t [@@deriving bin_io]

type t = {
  arch    : arch;
  core    : Kernel.t;
  disasm  : disasm;
  memory  : value memmap;
  storage : dict;
  program : program term;
  symbols : Symtab.t;
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
    Value.get Image.code_region v |> Option.is_some

  let is_data v = not (is_code v)

  let of_image finish file img = {
    arch = Image.arch img;
    data = Memmap.filter ~f:is_data (Image.memory img);
    code = Memmap.filter ~f:is_code (Image.memory img);
    file;
    finish;
  }

  let symtab_agent =
    let reliability = KB.Agent.authorative in
    KB.Agent.register "symtab"
      ~reliability
      ~desc:"extracts symbols from symbol tables"
      ~package:"bap"

  let provide_image image =
    let image_symbols = Symbolizer.of_image image in
    let image_roots = Rooter.of_image image in
    info "providing rooter and symbolizer from image of %a"
      Sexp.pp_hum ([%sexp_of : string option] (Image.filename image));
    Symbolizer.provide symtab_agent image_symbols;
    Rooter.provide image_roots

  let of_image ?loader filename =
    Image.create ?backend:loader filename >>| fun (img,warns) ->
    List.iter warns ~f:(fun e -> warning "%a" Error.pp e);
    let spec = Image.spec img in
    Signal.send Info.got_img img;
    Signal.send Info.got_spec spec;
    provide_image img;
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
      | Some load ->
        fun () -> load filename ()

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
    {arch; data; code; file = filename; finish = ident;}

  let available_loaders () =
    Hashtbl.keys loaders @ Image.available_backends ()
end

module Merge = struct

  let merge_streams ss ~f : 'a Source.t =
    Stream.concat_merge ss
      ~f:(fun s s' -> match s, s' with
          | Ok s, Ok s' -> Ok (f s s')
          | Ok _, Error er
          | Error er, Ok _ -> Error er
          | Error er, Error er' ->
            Error (Error.of_list [er; er']))

  let merge_sources create sources ~f = match sources with
    | [] -> None
    | names -> match List.filter_map names ~f:create with
      | [] -> assert false
      | ss -> Some (merge_streams ss ~f)

  let symbolizer () =
    let symbolizers = Symbolizer.Factory.list () in
    merge_sources Symbolizer.Factory.find symbolizers ~f:(fun s1 s2 ->
        Symbolizer.chain [s1;s2])

  let rooter () =
    let rooters = Rooter.Factory.list () in
    merge_sources Rooter.Factory.find rooters ~f:Rooter.union

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

let build ?package ?state ~file ~code ~data arch =
  let init = match state with
    | Some state -> Kernel.{state with package}
    | None -> Kernel.empty ?package arch in
  let kernel =
    let open KB.Syntax in
    set_package package >>= fun () ->
    Memmap.to_sequence code |> KB.Seq.fold ~init ~f:(fun k (mem,_) ->
        Kernel.update k mem) in
  let cfg,symbols,core = Kernel.Toplevel.run arch ~code ~data file kernel in
  {
    core;
    disasm = Disasm.create cfg;
    program = Program.lift symbols;
    symbols;
    arch; memory=union_memory code data;
    storage = Dict.set Dict.empty filename file;
    passes=[]
  }

let state {core} = core
let package {core={Kernel.package}} = package

let create_exn
    ?package
    ?state
    ?disassembler:_
    ?brancher:_
    ?symbolizer:_
    ?rooter:_
    ?reconstructor:_
    (read : input)  =
  let {Input.arch; data; code; file; finish} = read () in
  Signal.send Info.got_file file;
  Signal.send Info.got_arch arch;
  Signal.send Info.got_data data;
  Signal.send Info.got_code code;
  finish @@ build ?package ?state ~file ~code ~data arch

let create
    ?package ?state ?disassembler ?brancher ?symbolizer ?rooter ?
    reconstructor input =
  try
    Ok (create_exn
          ?package ?state ?disassembler ?brancher ?symbolizer ?rooter
          ?reconstructor input)
  with
  | Toplevel.Conflict err ->
    let open Error.Internal_repr in
    let msg =
      String (Format.asprintf "Knowledge Base Conflict: %a"
                KB.Conflict.pp err) in
    Error (to_info msg)
  | exn -> Or_error.of_exn ~backtrace:`Get exn

let restore_state _ =
  failwith "Project.restore_state: this function should no be used.
    Please use the Toplevel module to save/restore the state."

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

module Collator = struct
  open Bap_knowledge

  type info = {
    name : Knowledge.Name.t;
    desc : string option;
  }

  type t = Collator : {
      prepare : project -> 's;
      collate : int -> 's -> project -> 's;
      summary : 's -> unit;
    } -> t

  let registry = Hashtbl.create (module Knowledge.Name)

  let apply (Collator {prepare; collate; summary}) projects =
    match Seq.split_n projects 1 with
    | [base],rest ->
      summary @@
      Seq.foldi ~init:(prepare base) rest ~f:collate
    | _ -> ()

  let register ?desc ?package name ~prepare ~collate ~summary =
    let name = Knowledge.Name.create ?package name in
    if Hashtbl.mem registry name then
      invalid_argf "A collator with name %s is already registered \
                    please choose another unique name"
        (Knowledge.Name.show name) ();
    Hashtbl.add_exn registry name (desc,Collator {
        prepare;
        collate;
        summary;
      })

  let find ?package name =
    let name = Knowledge.Name.read ?package name in
    match Hashtbl.find registry name with
    | Some (_,x) -> Some x
    | None -> None

  let registered () =
    Hashtbl.to_alist registry |>
    List.map ~f:(fun (name,(desc,_)) -> {name; desc})


  let name {name} = name
  let desc = function
    | {desc=None} -> "not provided"
    | {desc=Some txt} -> txt
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
  Data.set_module_name instance "Bap.Std.Project";
  KB.Rule.(declare ~package:"bap" "project-filename" |>
           dynamic ["input"] |>
           dynamic ["data"; "code"; "path"] |>
           require Theory.Label.addr |>
           provide Theory.Label.path |>
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
