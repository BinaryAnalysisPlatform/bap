open Core_kernel
open Regular.Std
open Bap_types.Std
open Monads.Std
open Format
open Image_internal_std

module Fact = Ogre.Make(Monad.Ident)
module Result = Monad.Result.Error
open Result.Syntax

type path = string
type doc = Ogre.doc
module type Loader = sig
  val from_file : string -> doc option Or_error.t
  val from_data : Bigstring.t -> doc option Or_error.t
end

type loader = (module Loader)
let backends : loader String.Table.t =
  String.Table.create ()

type location = {
  addr : addr;
  size : int;
} [@@deriving bin_io, compare, sexp]

type 'a region = {
  name : string;
  locn : location;
  info : 'a;
} [@@deriving bin_io, compare, sexp]

type mapped = {
  off : int;
  len : int;
  endian : endian;
  r : bool;
  w : bool;
  x : bool;
} [@@deriving bin_io, compare, sexp]


type symbol_info = {
  kind : [`Code | `Debug | `Dyn] list;
  extra_locns : location list;
} [@@deriving bin_io, compare, sexp]

type symtab = symbol_info region table

let pp_region fmt {name; locn={addr; size}} =
  fprintf fmt "%s %a %d" name Addr.pp addr size

let hash_region {locn={addr}} = Addr.hash addr

module Segment = struct

  module T = struct
    type t = mapped region [@@deriving bin_io, compare, sexp]
    let hash = hash_region
    let pp = pp_region
    let module_name = Some "Bap.Std.Image.Segment"
    let version = "2.0.0"
  end

  let name t = t.name
  let addr t = t.locn.addr
  let size t = t.locn.size
  let is_writable {info={w}} = w
  let is_readable {info={r}} = r
  let is_executable {info={x}} = x
  include T
  include Regular.Make(T)
end

module Symbol = struct
  open! Polymorphic_compare
  module T = struct
    type t = symbol_info region [@@deriving bin_io,compare,sexp]
    let pp = pp_region and hash = hash_region
    let module_name = Some "Bap.Std.Image.Symbol"
    let version = "2.0.0"
  end
  let name {name} = name
  let is_debug {info={kind}} = List.mem ~equal kind `Debug
  let is_function {info={kind}} = List.mem ~equal kind `Code
  include T
  include Regular.Make(T)
end

type segment = Segment.t [@@deriving bin_io, compare, sexp]
type symbol = Symbol.t [@@deriving bin_io,compare, sexp]

module Spec = struct
  type t = {
    arch : arch;
    entry : addr;
    segments : segment list;
    symbols : symbol list;
    sections : unit region list;
    code  : mapped region list;
  } [@@deriving bin_io, compare, sexp]
end


type words = {
  r8  : word table Lazy.t;
  r16 : word table Lazy.t;
  r32 : word table Lazy.t;
  r64 : word table Lazy.t;
  r128 : word table Lazy.t;
  r256 : word table Lazy.t;
}

let sexp_of_doc doc = Sexp.Atom (Ogre.Doc.to_string doc)

type t = {
  doc  : doc;
  spec : Spec.t;
  name : string option;
  data : Bigstring.t;
  symbols : symbol table;
  segments : segment table;
  memory : value memmap;
  words : words sexp_opaque;
  memory_of_segment : segment -> mem sexp_opaque;
  memory_of_symbol : (symbol -> mem * mem seq) Lazy.t sexp_opaque;
  symbols_of_segment : (segment -> symbol seq) Lazy.t sexp_opaque;
  segment_of_symbol : (symbol -> segment) Lazy.t sexp_opaque;
} [@@deriving sexp_of]


type result = (t * Error.t list) Or_error.t


let segment = Value.Tag.register (module Segment)
    ~name:"segment"
    ~uuid:"a0eec123-5937-4283-b141-58d579a9b0df"

let symbol  = Value.Tag.register (module String)
    ~name:"symbol"
    ~uuid:"768bc13d-d4be-43fc-9f7a-c369ebab9c7e"

let function_start = Value.Tag.register (module Unit)
    ~name:"function-start"
    ~uuid:"1c1809ec-a38a-4aee-a46c-3d49127ba85a"

let symbol_info = Value.Tag.register (module Symbol)
    ~name:"symbol-info"
    ~uuid:"706e78cf-07de-4417-a422-effa7821dd02"

let section  = Value.Tag.register (module String)
    ~name:"section"
    ~uuid:"4014408d-a3af-488f-865b-5413beaf198e"

let code_region = Value.Tag.register (module Unit)
    ~name:"code-region"
    ~uuid:"5296b7e9-d5c3-40ce-98ba-7655ec68fad7"

let file = Value.Tag.register (module String)
    ~name:"file"
    ~uuid:"c119f700-4069-47ad-ba99-fc29791e0d47"

let specification = Value.Tag.register (module Bap_ogre.Doc)
    ~name:"image-specification"
    ~uuid:"a0c98f1f-3693-412a-a11a-2b6c3f6935a7"

let mem_of_locn mem {addr;size} : mem Or_error.t =
  match Memory.view ~from:addr ~words:size mem with
  | Error err ->
    Result.failf "region %a+%d can't be mapped into memory, %a"
      Addr.pp addr size Error.pp err ()
  | ok -> ok

let tag mem tag value memmap =
  Memmap.add memmap mem (Value.create tag value)


let map_region data {locn={addr}; info={off; len; endian}} =
  Memory.create ~pos:off ~len endian addr data

let static_view segments = function {addr} as locn ->
match Table.find_addr segments addr with
| None -> Result.failf "region is not mapped to memory" ()
| Some (segmem,_) -> mem_of_locn segmem locn

let add_sym segments memory (symtab : symtab)
    ({name; locn=entry; info={extra_locns=locns}} as sym) =
  static_view segments entry >>= fun _entry_region ->
  Result.List.fold (entry::locns) ~init:(memory,symtab)
    ~f:(fun (memory,symtab) locn ->
        static_view segments locn >>= fun mem ->
        let memory = tag mem symbol name memory |>
                     tag mem symbol_info sym in
        match Table.add symtab mem sym with
        | Ok symtab -> Ok (memory,symtab)
        | _intersects_ -> Ok (memory,symtab))

let add_segment base memory segments seg =
  map_region base seg >>= fun mem ->
  Table.add segments mem seg >>= fun segments ->
  let memory = tag mem segment seg memory in
  Result.return (memory,segments)

let add_region kind base memory regions reg =
  map_region base reg >>= fun mem ->
  Table.add regions mem reg >>= fun regions ->
  let memory = tag mem kind reg memory in
  Result.return (memory,regions)

let add_code base memory regions reg =
  map_region base reg >>= fun mem ->
  Table.add regions mem reg >>= fun regions ->
  let memory = tag mem code_region () memory in
  Result.return (memory,regions)

let add_sections_view segments sections memmap =
  List.fold sections ~init:(memmap,[])
    ~f:(fun (memmap,ers) {name; locn}  ->
        match static_view segments locn with
        | Ok mem -> tag mem section name memmap, ers
        | Error er -> memmap,er::ers)

let make_table add base memory =
  List.fold ~init:(memory,Table.empty,[])
    ~f:(fun (memory,table,warns) sym ->
        match add base memory table sym with
        | Ok (memory,table) -> (memory,table,warns)
        | Error happens -> (memory,table,happens::warns))

let make_symtab = make_table add_sym
let make_segtab = make_table add_segment
let make_codetab = make_table add_code

(** [words_of_table word_size table] maps all memory mapped by [table]
    to words of size [word_size]. If size of mapped region is not enough
    to fit the word, then it is ignored.
    We can ignore errors from [Table.add] since precondition that
    memory regions doesn't overlap is enforced by [Table.fold] and
    [Memory.fold].  *)
let words_of_table word_size tab =
  let words_of_memory mem tab =
    Memory.foldi ~word_size mem ~init:tab
      ~f:(fun addr word tab ->
          match Memory.view ~word_size ~from:addr ~words:1 mem with
          | Error _ -> tab
          | Ok mem  -> ok_exn (Table.add tab mem word)) in
  Table.foldi tab ~init:Table.empty ~f:(fun mem _ -> words_of_memory mem)

let create_words secs = {
  r8  = lazy (words_of_table `r8  secs);
  r16 = lazy (words_of_table `r16 secs);
  r32 = lazy (words_of_table `r32 secs);
  r64 = lazy (words_of_table `r64 secs);
  r128 = lazy (words_of_table `r128 secs);
  r256 = lazy (words_of_table `r256 secs);
}

let available_backends () = Hashtbl.keys backends

let create_segment_of_symbol_table syms secs =
  let tab = Symbol.Table.create ()
      ~growth_allowed:false
      ~size:(Table.length syms) in
  Table.iteri syms ~f:(fun mem sym ->
      match Table.find_addr secs (Memory.min_addr mem) with
      | None -> ()
      | Some (_,sec) ->
        Symbol.Table.add_exn tab ~key:sym ~data:sec);
  Symbol.Table.find_exn tab

let from_spec query base doc =
  Fact.eval query doc >>= function
    {Spec.segments; symbols; sections;code} as spec ->
    let memory = Memmap.empty in
    let memory,segs,seg_warns = make_segtab base memory segments in
    let memory,syms,sym_warns = make_symtab segs memory symbols in
    let memory,sec_warns = add_sections_view segs sections memory in
    let memory,_,creg_warns = make_codetab base memory code in
    let words = create_words segs in
    Table.(rev_map ~one_to:one Segment.hashable (segs : segment table)) >>=
    fun (memory_of_segment : segment -> mem) ->
    let memory_of_symbol () : symbol -> mem * mem seq =
      ok_exn (Table.(rev_map ~one_to:at_least_one Symbol.hashable syms)) in
    let symbols_of_segment () : segment -> symbol seq =
      Table.(link ~one_to:many Segment.hashable segs syms) in
    let segment_of_symbol () : symbol -> segment =
      create_segment_of_symbol_table syms segs in
    Result.return ({
        doc; spec;
        name = None; data=base; symbols=syms; segments=segs; words;
        memory_of_segment;
        memory;
        memory_of_symbol   = Lazy.from_fun memory_of_symbol;
        symbols_of_segment = Lazy.from_fun symbols_of_segment;
        segment_of_symbol  = Lazy.from_fun segment_of_symbol;
      }, (seg_warns @ sym_warns @ sec_warns @ creg_warns))

let data t = t.data
let memory t = t.memory

let entry_point {spec={Spec.entry}} = entry
let filename t = t.name
let arch {spec={Spec.arch}} = arch
let addr_size t = Arch.addr_size (arch t)
let endian t = Arch.endian (arch t)
let spec t = t.doc

let words t (size : size) : word table =
  let lazy table = match size with
    | `r8  -> t.words.r8
    | `r16 -> t.words.r16
    | `r32 -> t.words.r32
    | `r64 -> t.words.r64
    | `r128 -> t.words.r128
    | `r256 -> t.words.r256 in
  table

let segments t = t.segments
let symbols t = t.symbols
let memory_of_segment t = t.memory_of_segment

let memory_of_symbol {memory_of_symbol = lazy f} = f
let symbols_of_segment {symbols_of_segment = lazy f} = f
let segment_of_symbol {segment_of_symbol = lazy f} = f
let register_loader ~name backend =
  Hashtbl.add_exn backends ~key:name ~data:backend

module Scheme = struct
  open Ogre.Type
  type addr = int64
  type size = int64
  type off = int64
  type 'a region = {addr : int64; size: int64; info : 'a}
  let region addr size info = {addr; size; info}
  let void_region addr size = {addr; size; info = ()}

  let off  = "off"  %: int
  let size = "size" %: int
  let addr = "addr" %: int
  let name = "name" %: str
  let root = "root" %: int
  let readable   = "r" %: bool
  let writable   = "w" %: bool
  let executable = "x" %: bool
  let fixup = "fixup" %: int

  let location () = scheme addr $ size
  let declare name scheme f = Ogre.declare ~name scheme f
  let named n scheme f = declare n (scheme $ name) f
  let arch    () = declare "arch" (scheme name) ident
  let section () = declare "section" (location ()) void_region
  let code_start   () = declare "code-start" (scheme addr) ident
  let entry_point  () = declare "entry-point" (scheme addr) ident
  let symbol_chunk () = declare "symbol-chunk" (location () $ root) region
  let named_region () = named "named-region" (location ()) region
  let named_symbol () = named "named-symbol" (scheme addr) (fun x y -> x,y)
  let rwx scheme = scheme $ readable $ writable $ executable
  let segment () = declare "segment" (location () |> rwx)
      (fun addr size r w x -> {addr; size; info=(r,w,x)})
  let mapped () = declare "mapped" (location () $off)
      (fun addr size off -> region addr size off)

  let code_region () =
    declare "code-region" (scheme addr $ size $ off) Tuple.T3.create

  let relocation () =
    declare "relocation" (scheme fixup $ addr) Tuple.T2.create
  let external_reference () =
    declare "external-reference" (scheme addr $ name) Tuple.T2.create
  let base_address () = declare "base-address" (scheme addr) ident
end


module Derive = struct
  open Fact.Syntax
  open Scheme

  let int_of_int64 x = match Int64.to_int x with
    | None -> Fact.failf "unable to convert int64 to int" ()
    | Some x -> Fact.return x

  let arch =
    Fact.collect Ogre.Query.(select (from arch)) >>= fun s ->
    Fact.Seq.reduce ~f:(fun a1 a2 ->
        if Arch.equal a1 a2 then Fact.return a1
        else Fact.failf "arch is ambiguous" ())
      (Seq.filter_map ~f:Arch.of_string s) >>= fun a ->
    match a with
    | Some a -> Fact.return a
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let addr_width = arch >>| Arch.addr_size >>| Size.in_bits

  let endian = arch >>| Arch.endian

  let entry =
    addr_width >>= fun width ->
    Fact.require entry_point >>| Word.of_int64 ~width

  let location ~addr ~size : location Fact.t =
    addr_width >>= fun width ->
    let addr = Word.of_int64 ~width addr in
    int_of_int64 size >>| fun size ->
    {addr;size}


  let segments : segment seq Fact.t =
    endian >>= fun endian ->
    Fact.foreach Ogre.Query.(begin
        select (from segment $ mapped $ named_region)
          ~join:[[field addr];
                 [field size ~from:segment;
                  field size ~from:named_region]]
      end) ~f:(fun
                {addr; size; info=(r,w,x)}
                {size=len; info=off}
                {info=name} ->
                location ~addr ~size >>= fun locn ->
                int_of_int64 off >>= fun off ->
                int_of_int64 len >>| fun len ->
                {name; locn; info={off; len; endian; r; w; x}}) >>=
    Fact.Seq.all

  let sections =
    Fact.foreach Ogre.Query.(begin
        select (from section $ named_region)
          ~join:[[field addr];
                 [field size ~from:section;
                  field size ~from:named_region]]
      end) ~f:(fun {addr; size;} {info=name} ->
        location ~addr ~size >>| fun locn ->
        {locn; name; info=()}) >>=
    Fact.Seq.all

  let symbol_locations start =
    Fact.foreach Ogre.Query.(begin
        select ~where:(symbol_chunk.(root) = int start)
          (from symbol_chunk)
      end) ~f:(fun {addr; size} -> location ~addr ~size) >>=
    Fact.Seq.all

  let symbol_name start =
    Fact.foreach Ogre.Query.(begin
        select ~where:(named_symbol.(addr) = int start)
          (from named_symbol)
      end) ~f:snd >>|
    Seq.fold ~init:None ~f:(fun name n -> match name with
        | None -> Some n
        | Some name as r when String.length n < String.length name -> r
        | _ -> Some n)

  let symbols =
    Fact.foreach Ogre.Query.(begin
        select ~join:[[field addr]] (from code_start)
      end) ~f:(fun start ->
        symbol_name start >>= function
        | None -> Fact.return None
        | Some name ->
          symbol_locations start >>= fun locations ->
          match Seq.to_list locations with
          | [] -> Fact.return None
          | locn::locns ->
            Fact.return (Some {
                name; locn; info={kind=[`Code]; extra_locns = locns}
              })) >>=
    Fact.Seq.all >>| Seq.filter_opt

  let code =
    endian >>= fun endian ->
    Fact.foreach Ogre.Query.(begin
        select (from code_region $ named_region)
          ~join:[[field addr];]
      end) ~f:(fun (addr,size, off) {info=name} ->
        location ~addr ~size >>= fun locn ->
        int_of_int64 off  >>= fun off ->
        int_of_int64 size >>| fun len ->
        {locn; name; info={endian;off;len;r=true;w=false;x=true}}) >>=
    Fact.Seq.all

  let image =
    arch >>= fun arch ->
    entry    >>= fun entry ->
    segments >>= fun segments ->
    sections >>= fun sections ->
    code >>= fun code ->
    symbols  >>| fun symbols  -> {
      Spec.arch; entry;
      segments = Seq.to_list_rev segments;
      symbols  = Seq.to_list_rev symbols;
      sections = Seq.to_list_rev sections;
      code = Seq.to_list_rev code;
    }
end


module Legacy = struct
  open Fact.Syntax
  open Scheme
  open Image_backend

  let rec is p = function
    | Or (p1,p2) -> is p p1 || is p p2
    | p' -> p = p'

  let addr x = ok_exn (Word.to_int64 x)

  let location_repr {Location.addr=x; len} =
    addr x, Int64.of_int len

  let vsize secs {Segment.name; location={Location.addr;len}} =
    List.find_map secs
      ~f:(fun {Section.name=n; location={Location.addr=a;len}} ->
          if name = n && addr = a then Some len else None) |> function
    | None -> len
    | Some len -> len

  let provide_segment sections s =
    let addr,fsize = location_repr @@ Segment.location s in
    let perm = Segment.perm s in
    let r,w,x = is R perm, is W perm, is X perm in
    let off = Int64.of_int (Segment.off s) in
    let vsize = Int64.of_int (vsize sections s) in
    Fact.provide segment addr vsize r w x >>= fun () ->
    Fact.provide mapped addr fsize off >>= fun () ->
    Fact.provide named_region addr vsize (Segment.name s)

  let provide_section s =
    let addr, size = location_repr @@ Section.location s in
    Fact.provide section addr size >>= fun () ->
    Fact.provide named_region addr size (Section.name s)

  let provide_function s =
    let loc, locs = Symbol.locations s in
    let root,size = location_repr loc in
    Fact.provide named_symbol root (Symbol.name s) >>= fun () ->
    Fact.provide code_start root >>= fun () ->
    Fact.provide symbol_chunk root size root >>= fun () ->
    Fact.List.iter locs ~f:(fun loc ->
        let addr,size = location_repr loc in
        Fact.provide symbol_chunk addr size root)

  let provide_symbol s =
    if Symbol.is_function s then provide_function s
    else Fact.return ()

  let provide_image
      {Img.arch=a; entry; segments=(s,ss); sections; symbols} =
    Fact.provide arch (Arch.to_string a) >>= fun () ->
    Fact.provide entry_point (addr entry) >>= fun () ->
    Fact.List.iter (s::ss)  ~f:(provide_segment sections) >>= fun () ->
    Fact.List.iter sections ~f:provide_section >>= fun () ->
    Fact.List.iter symbols  ~f:provide_symbol
end


let register_backend ~name load =
  let module Loader = struct
    let from_data input = match load input with
      | None ->
        Result.failf
          "A legacy %s loader was \
           unable to process input" name ()
      | Some img ->
        match Fact.exec (Legacy.provide_image img) Ogre.Doc.empty with
        | Error err ->
          Result.failf
            "A legacy %s loader failed to provide data - %a"
            name Error.pp err ()
        | Ok spec ->  Ok (Some spec)

    let from_file filename =
      from_data (Bap_fileutils.readfile filename)
  end in
  if Hashtbl.mem backends name then `Duplicate
  else (register_loader ~name (module Loader); `Ok)


module Metaloader () = struct

  let merge_docs d1 d2 = match Ogre.Doc.merge d1 d2 with
    | Ok d3 -> Ok d3
    | Error _ ->
      if Ogre.Doc.declarations d1 >
         Ogre.Doc.declarations d2
      then Ok d1
      else if Ogre.Doc.definitions d1 >
              Ogre.Doc.definitions d2
      then Ok d1
      else Ok d2

  let load invoke =
    Hashtbl.data backends |>
    List.fold ~init:(Ok None) ~f:(fun doc loader ->
        match doc, invoke loader with
        | Ok None,doc
        | doc,Ok None -> doc
        | Ok (Some doc), Error _
        | Error _ , Ok (Some doc) -> Ok (Some doc)
        | Error e1, Error e2 -> Error (Error.of_list [e1;e2])
        | Ok Some d1, Ok Some d2 ->
          merge_docs d1 d2 |> Or_error.map ~f:Option.some)


  let from_file name =
    load (fun (module Loader) -> Loader.from_file name)
  let from_data data =
    load (fun (module Loader) -> Loader.from_data data)
end

let from_spec = from_spec Derive.image

let merge_loaders () : loader =
  let module Loader = Metaloader () in
  (module Loader)

let get_loader = function
  | None -> merge_loaders ()
  | Some name -> match Hashtbl.find backends name with
    | Some loader -> loader
    | None ->
      let module Failure = struct
        let fail _ = Result.failf "Unknown image loader %s" name ()
        let from_file = fail
        let from_data = fail
      end in
      (module Failure)

let invoke load data arg = match load arg with
  | Ok (Some doc) -> from_spec (data arg) doc
  | Ok None ->
    Result.failf "input file format is not supported \
                  by the specified loader" ()
  | Error _ as err -> err

let result_with_filename file (t,warns) =
  ({t with name = Some file},warns)

let create ?backend path =
  let (module Load) = get_loader backend in
  invoke Load.from_file Bap_fileutils.readfile path >>|
  result_with_filename path

let of_bigstring ?backend data =
  let (module Load) = get_loader backend in
  invoke Load.from_data ident data

let of_string ?backend data =
  let (module Load) = get_loader backend in
  invoke Load.from_data ident (Bigstring.of_string data)
