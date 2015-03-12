open Core_kernel.Std
open Bap_types.Std
open Or_error

open Image_common
open Image_internal_std
open Backend

type 'a m = 'a Or_error.t
type img = Backend.Img.t with sexp_of
type path = string

let backends : Backend.t String.Table.t =
  String.Table.create ()


let (+>) = Fn.compose

(* We hide Sec and Sym module making them not only abstract,
   but even nonconstructable from the outside, so that we
   can enforce several invariants.  In particular, we can enforce
   a strict bijection between section and memory regions as well
   as between symbol and memory. This means that no one can construct
   such symbol or section that will not be mapped to the memory. This
   allows us to provide handy mapping functions, that are guaranteed
   for not to fail.
*)

module Sec = struct
  module T = struct
    type t = Section.t with bin_io, compare, sexp
    let hash = Addr.hash +> Location.addr +> Section.location
    let pp fmt t = Format.fprintf fmt "%s" @@ Section.name t
    let module_name = "Bap_image.Sec"
  end

  let name = Section.name


  let rec checked p = function
    | Or (p1,p2) -> checked p p1 || checked p p2
    | X|W|R as p' -> p = p'

  let is_writable t   = checked W (Section.perm t)
  let is_readable t   = checked R (Section.perm t)
  let is_executable t = checked X (Section.perm t)
  include T
  include Regular.Make(T)
end

module Sym = struct
  module T = struct
    type t = Sym.t with bin_io, compare, sexp
    let hash = Addr.hash +> Location.addr +> fst +> Sym.locations
    let pp fmt t = Format.fprintf fmt "%s" @@ Sym.name t
    let module_name = "Bap_image.Sym"
  end
  include Sym
  include Regular.Make(T)
end

type sec = Sec.t with bin_io, compare, sexp
type sym = Sym.t with bin_io, compare, sexp

type words = {
  r8  : word table Lazy.t;
  r16 : word table Lazy.t;
  r32 : word table Lazy.t;
  r64 : word table Lazy.t;
}

type t = {
  img  : img ;
  name : string option;
  data : Bigstring.t;
  symbols : sym table;
  sections : sec table;
  tags : (string * string) memmap;
  words : words sexp_opaque;
  memory_of_section : sec -> mem sexp_opaque;
  memory_of_symbol : (sym -> mem * mem seq) Lazy.t sexp_opaque;
  symbols_of_section : (sec -> sym seq) Lazy.t sexp_opaque;
  section_of_symbol : (sym -> sec) Lazy.t sexp_opaque;
} with sexp_of

type result = (t * Error.t list) Or_error.t


let mem_of_location endian data {Location.addr; len} =
  Memory.create ~len endian addr data

let find_section secs addr =
  Table.find_mapi secs ~f:(fun mem sec ->
      if Memory.contains mem addr then Some (mem,sec) else None)

module Error = struct
  include Error
  let prepend msg v sov err = tag_arg err msg v sov
end

let add_sym errs secs syms sym =
  let (m,ms) = Sym.locations sym in
  List.fold (m::ms) ~init:(syms,errs) ~f:(fun (map,errs) loc ->
      let (addr, len) = Location.(addr loc, len loc) in
      let r = match find_section secs addr with
        | None -> Error Error.(of_string "no section for location")
        | Some (mem,sec) ->
          match Memory.view ~from:addr ~words:len mem with
          | Error err ->
            Error
              Error.(tag_arg err "section memory"
                       (lazy (Memory.min_addr mem, Memory.max_addr mem))
                       <:sexp_of<(addr*addr) Lazy.t>> |>
                     prepend "symbol's memory doesn't fit in section"
                       sec sexp_of_sec)
          | Ok mem -> match Table.add map mem sym with
            | Error err -> Error Error.(tag err "intersecting symbol")
            | ok -> ok in
      match r with
      | Ok map -> (map,errs)
      | Error err ->
        let err = Error.tag_arg err "skipped sym" sym sexp_of_sym in
        map, err::errs)

let create_symbols syms secs =
  List.fold syms ~init:(Table.empty,[])
    ~f:(fun (symtab,errs) sym -> add_sym errs secs symtab sym )

let validate_no_intersections (s,ss) : Validate.t =
  let open Backend.Section in
  let (vs,_) = List.fold ss ~init:([],s) ~f:(fun (vs,s1) s2 ->
      let loc1,loc2 = location s1, location s2 in
      let open Location in
      let v1 =
        Addr.(validate_lbound  loc2.addr
                ~min:(Incl (loc1.addr ++ loc1.len))) in
      let v2 =
        Int.validate_lbound s2.off ~min:(Incl (s1.off + loc1.len))  in
      (v1 :: v2 :: vs, s2)) in
  Validate.name_list "sections shouldn't intersect" vs


let validate_section data s : Validate.t =
  let size = Bigstring.length data in
  let off = Section.off s in
  let len = Location.len (Section.location s) in
  Validate.(name_list "section" [
      name "offset" @@
      Int.validate_bound off ~min:(Incl 0) ~max:(Excl size);
      name "length" @@
      Int.validate_bound len ~min:(Incl 1) ~max:(Incl size);
      name "offset+length" @@
      Int.validate_ubound (len+off) ~max:(Incl size)
    ])

let validate_sections data (s,ss) : Validate.t =
  let check_secs = Validate.list_indexed (validate_section data) in
  Validate.of_list [
    check_secs (s::ss);
    validate_no_intersections (s,ss)
  ]

let create_sections arch data (s,ss) =
  let endian = Arch.endian arch in
  Validate.result (validate_sections data (s,ss)) >>= fun () ->
  List.fold (s::ss) ~init:(return Table.empty) ~f:(fun tab s ->
      let loc, pos = Section.(location s, off s) in
      let len, addr = Location.(len loc, addr loc) in
      Memory.create ~pos ~len endian addr data >>= fun mem ->
      tab >>= fun tab -> Table.add tab mem s)

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
          | Error err ->
            eprintf "\nSkipping with error: %s\n"
              (Error.to_string_hum err);
            tab
          | Ok mem  -> ok_exn (Table.add tab mem word)) in
  Table.foldi tab ~init:Table.empty ~f:(fun mem _ -> words_of_memory mem)

let create_words secs = {
  r8  = lazy (words_of_table `r8  secs);
  r16 = lazy (words_of_table `r16 secs);
  r32 = lazy (words_of_table `r32 secs);
  r64 = lazy (words_of_table `r64 secs);
}

let register_backend ~name backend =
  String.Table.add backends ~key:name ~data:backend

let of_img img data name =
  let open Img in
  let arch = arch img in
  create_sections arch data (sections img) >>= fun secs ->
  let syms,errs = create_symbols (symbols img) secs in
  let words = create_words secs in
  Table.(rev_map ~one_to:one Sec.hashable secs)
  >>= fun (memory_of_section : sec -> mem) ->
  let memory_of_symbol () : sym -> mem * mem seq =
    ok_exn (Table.(rev_map ~one_to:at_least_one Sym.hashable syms)) in
  let symbols_of_section () : sec -> sym seq =
    Table.(link ~one_to:many Sec.hashable secs syms) in
  let section_of_symbol () : sym -> sec =
    Table.(link ~one_to:one Sym.hashable syms secs) in
  let tags = List.fold (tags img) ~init:Memmap.empty ~f:(fun map tag ->
      match mem_of_location (Arch.endian arch) data tag.Tag.location with
      | Ok mem -> Memmap.add map mem Tag.(tag.name, tag.data)
      | Error _ ->  map) in
  return ({
      img; name; data; symbols = syms; sections = secs; words;
      memory_of_section;
      tags;
      memory_of_symbol   = Lazy.from_fun memory_of_symbol;
      symbols_of_section = Lazy.from_fun symbols_of_section;
      section_of_symbol  = Lazy.from_fun section_of_symbol;
    }, errs)

let data t = t.data
let tags t = t.tags


let of_backend backend data path : result =
  match String.Table.find backends backend with
  | None -> errorf "no such backend: '%s'" backend
  | Some load -> match load data with
    | Some img -> of_img img data path
    | None -> error "create image" (backend,`path path)
                <:sexp_of<string * [`path of string option]>>

let autoload data path =
  let bs = String.Table.data backends in
  match List.filter_map bs ~f:(fun load -> load data) with
  | [img] -> of_img img data path
  | [] -> errorf "Autoloader: no suitable backend found"
  | _  -> errorf "Autoloader: can't resolve proper backend"

let create_image path ?backend data : result =
  match backend with
  | None -> autoload data path
  | Some backend -> of_backend backend data path

let of_bigstring ?backend data =
  create_image None ?backend data

let of_string ?backend data =
  of_bigstring ?backend (Bigstring.of_string data)

let create ?backend path : result =
  try_with (fun () -> Bap_fileutils.readfile path) >>= fun data ->
  match backend with
  | None -> autoload data (Some path)
  | Some backend -> of_backend backend data (Some path)

let entry_point t = Img.entry t.img
let filename t = t.name
let arch t = Img.arch t.img
let addr_size t = Arch.addr_size (Img.arch t.img)
let endian t = Arch.endian (Img.arch t.img)

let words t (size : size) : word table =
  let lazy table = match size with
    | `r8  -> t.words.r8
    | `r16 -> t.words.r16
    | `r32 -> t.words.r32
    | `r64 -> t.words.r64 in
  table

let sections t = t.sections
let symbols t = t.symbols
let memory_of_section t = t.memory_of_section

let memory_of_symbol {memory_of_symbol = lazy f} = f
let symbols_of_section {symbols_of_section = lazy f} = f
let section_of_symbol {section_of_symbol = lazy f} = f

TEST_MODULE = struct

  let expect ?(print=false) ~errors data_size ss =
    let width = 32 in
    let section (addr, off, size) = {
      Section.name = "test-section";
      Section.perm = R;
      Section.off;
      Section.location = {
        Location.addr = Addr.of_int ~width addr;
        Location.len = size;

      }
    } in
    let data = Bigstring.create data_size in
    let secs = match List.map ss ~f:section with
      | [] -> invalid_arg "empty set of sections"
      | (s::ss) -> (s,ss) in
    let v = validate_sections data secs in
    if print then begin
      match Validate.(result v) with
      | Ok () -> eprintf "No errors\n"
      | Error err -> eprintf "Errors: %s\n" @@ Error.to_string_hum err
    end;
    let has_errors = Validate.errors v <> [] in
    errors = has_errors

  TEST = expect ~errors:true  0 [0,0,0]
  TEST = expect ~errors:true  1 [0,0,0]
  TEST = expect ~errors:false 1 [0,0,1]
  TEST = expect ~errors:true  8 [0,0,4; 2,4,4]
  TEST = expect ~errors:true  8 [0,0,4; 4,0,4]
  TEST = expect ~errors:true  7 [0,0,4; 4,4,4]
  TEST = expect ~errors:true  8 [0,0,4; 4,1,4]
  TEST = expect ~errors:true  8 [0,2,4; 4,0,4]
  TEST = expect ~errors:true  8 [0,1,4; 4,0,4]
  TEST = expect ~errors:true  8 [0,0,4; 4,1,4]
end
