open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Monads.Std
open Or_error

open Image_common
open Image_internal_std
open Backend

module Bap_image_ogre = Bap_image_ogre.Make(Monad.Ident)
module EM = Monad.Result.Error

type 'a m = 'a Or_error.t
type img = Backend.Img.t [@@deriving sexp_of]
type path = string

type ogrefied_backend = Bigstring.t -> Ogre.doc option Or_error.t

let backends : ogrefied_backend String.Table.t =
  String.Table.create ()


let (+>) = Fn.compose

(* We hide Segment and Symbol types making them not only abstract,
   but even nonconstructable from the outside, so that we
   can enforce several invariants.  In particular, we can enforce
   a strict bijection between segment and memory regions as well
   as between symbol and memory. This means that no one can construct
   such symbol or segment that will not be mapped to the memory. This
   allows us to provide handy mapping functions, that are guaranteed
   for not to fail.
*)

module Segment = struct
  module T = struct
    type t = Segment.t [@@deriving bin_io, compare, sexp]
    let hash = Addr.hash +> Location.addr +> Segment.location
    let pp fmt t = Format.fprintf fmt "%s" @@ Segment.name t
    let module_name = Some "Bap.Std.Image.Segment"
    let version = "1.0.0"
  end

  let name = Segment.name


  let rec checked p = function
    | Or (p1,p2) -> checked p p1 || checked p p2
    | X|W|R as p' -> p = p'

  let is_writable t   = checked W (Segment.perm t)
  let is_readable t   = checked R (Segment.perm t)
  let is_executable t = checked X (Segment.perm t)
  include T
  include Regular.Make(T)
end

module Symbol = struct
  module T = struct
    type t = Symbol.t [@@deriving bin_io, compare, sexp]
    let hash = Addr.hash +> Location.addr +> fst +> Symbol.locations
    let pp fmt t = Format.fprintf fmt "%s" @@ Symbol.name t
    let module_name = Some "Bap.Std.Image.Symbol"
    let version = "1.0.0"
  end
  include Symbol
  include Regular.Make(T)
end

type segment = Segment.t [@@deriving bin_io, compare, sexp]
type symbol = Symbol.t [@@deriving bin_io, compare, sexp]

let segment = Value.Tag.register (module Segment)
    ~name:"segment"
    ~uuid:"a0eec123-5937-4283-b141-58d579a9b0df"

let symbol  = Value.Tag.register (module String)
    ~name:"symbol"
    ~uuid:"768bc13d-d4be-43fc-9f7a-c369ebab9c7e"

let section  = Value.Tag.register (module String)
    ~name:"section"
    ~uuid:"4014408d-a3af-488f-865b-5413beaf198e"

let file = Value.Tag.register (module String)
    ~name:"file"
    ~uuid:"c119f700-4069-47ad-ba99-fc29791e0d47"




type words = {
  r8  : word table Lazy.t;
  r16 : word table Lazy.t;
  r32 : word table Lazy.t;
  r64 : word table Lazy.t;
  r128 : word table Lazy.t;
  r256 : word table Lazy.t;
}

type t = {
  img  : img ;
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

let memory_error ?here msg mem =
  Error.create ?here msg
    (Memory.min_addr mem, Memory.max_addr mem)
    [%sexp_of:(addr*addr)]

let (++!) e1 e2 = Error.of_list [e1;e2]

let memory_of_location loc mem : mem Or_error.t =
  let (addr, len) = Location.(addr loc, len loc) in
  match Memory.view ~from:addr ~words:len mem with
  | Error err ->
    Error.create "symbol at" addr sexp_of_addr ++!
    Error.create "with size" len sexp_of_int ++!
    memory_error "doesn't fit into memory" mem ++! err |>
    Result.fail
  | ok -> ok

let add_sym memory errs secs syms sym =
  let (m,ms) = Symbol.locations sym in
  List.fold (m::ms) ~init:(memory,syms,errs)
    ~f:(fun (memory, map,errs) loc ->
        let (addr, len) = Location.(addr loc, len loc) in
        let memory,r = match Table.find_addr secs addr with
          | None ->
            memory, Error Error.(of_string "no segment for location")
          | Some (sec_mem,sec) ->
            match memory_of_location loc sec_mem with
            | Error err -> memory, Error err
            | Ok mem ->
              let memory =
                let tag = Value.create symbol (Symbol.name sym) in
                Memmap.add memory mem tag in
              match Table.add map mem sym with
              | Error err ->
                memory,Error Error.(tag err "intersecting symbol")
              | ok -> memory,ok in
        match r with
        | Ok map -> memory,map,errs
        | Error err ->
          let err = Error.tag_arg err "skipped sym" sym sexp_of_symbol in
          memory, map, err::errs)

let create_symbols memory syms secs =
  List.fold syms ~init:(memory,Table.empty,[])
    ~f:(fun (memory,symtab,errs) sym ->
        if Symbol.is_function sym
        then add_sym memory errs secs symtab sym
        else (memory,symtab,errs))

let validate_no_intersections (s,ss) : Validate.t =
  let open Backend.Segment in
  let (vs,_) = List.fold ss ~init:([],s) ~f:(fun (vs,s1) s2 ->
      let loc1,loc2 = location s1, location s2 in
      let open Location in
      let problem = sprintf "%s memory overlaps with %s" s1.name s2.name in
      let v1 = Validate.name problem @@ Addr.validate_lbound
          loc2.addr ~min:(Incl Addr.(loc1.addr ++ loc1.len)) in
      (* let v2 = *)
      (*   Int.validate_lbound s2.off ~min:(Incl (s1.off + loc1.len))  in *)
      (v1 :: vs, s2)) in
  Validate.name_list "segments shouldn't intersect" vs

let validate_segment data s : Validate.t =
  let size = Bigstring.length data in
  let off = Backend.Segment.off s in
  let len = Location.len (Backend.Segment.location s) in
  Validate.(name_list "segment" [
      name "offset" @@
      Int.validate_bound off ~min:(Incl 0) ~max:(Excl size);
      name "length" @@
      Int.validate_bound len ~min:(Incl 1) ~max:(Incl size);
      name "offset+length" @@
      Int.validate_ubound (len+off) ~max:(Incl size)
    ])

let validate_segments data (s,ss) : Validate.t =
  let check_secs = Validate.list_indexed (validate_segment data) in
  Validate.of_list [
    check_secs (s::ss);
    validate_no_intersections (s,ss)
  ]

let is_virtual s = Backend.Segment.off s < 0

let create_segments arch data (s,ss) =
  let endian = Arch.endian arch in
  (* a segment with the negative offset is virtual, we can't represent
     them in the Image, since the are not backed by static memory. But
     we will keep them, for analyses that require them.  *)
  List.filter (s::ss) ~f:(Fn.non is_virtual) |> function
  | [] -> return Table.empty
  | s::ss ->
  Validate.result (validate_segments data (s,ss)) >>= fun () ->
  List.fold (s::ss) ~init:(return Table.empty) ~f:(fun tab s ->
      let {Location.len; addr}, pos =
        Backend.Segment.(location s, off s) in
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
  r128 = lazy (words_of_table `r128 secs);
  r256 = lazy (words_of_table `r256 secs);
}

let register_loader ~name backend =
  match String.Table.add backends ~key:name ~data:backend with
  | `Ok -> ()
  | `Duplicate ->
    raise (Invalid_argument (sprintf "%s loader is already in use" name))

let register_backend ~name backend =
  let load str = match backend str with
    | None ->
      Or_error.error_string @@
      sprintf
        "file corrupted or %s backend is not able to process it" name
    | Some img ->
      match Bap_image_ogre.doc_of_image img with
      | Error er as r -> r
      | Ok doc -> Ok (Some doc) in
  try
    register_loader ~name load; `Ok
  with Invalid_argument _ -> `Duplicate

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

let backend_image t = t.img

let of_img img data name =
  let open Img in
  let arch = arch img in
  create_segments arch data (segments img) >>= fun secs ->
  let memory = Table.foldi secs ~init:Memmap.empty ~f:(fun mem sec map ->
      Memmap.add map mem (Value.create segment sec)) in
  let memory,syms,errs = create_symbols memory (symbols img) secs in
  let memory,errs =
    List.fold (sections img) ~init:(memory,errs) ~f:(fun (map,es) sec ->
        let loc = Section.location sec in
        let name = Section.name sec in
        let (addr,len) = Location.(addr loc, len loc) in
        Memmap.lookup memory addr |> Seq.hd |> function
        | None -> map,errs
        | Some (mem,_) -> match memory_of_location loc mem with
          | Error e ->
            map, Error.tag e ("skipped section " ^ name) :: es
          | Ok mem ->
            Memmap.add map mem (Value.create section name),es) in
  let words = create_words secs in
  Table.(rev_map ~one_to:one Segment.hashable secs)
  >>= fun (memory_of_segment : segment -> mem) ->
  let memory_of_symbol () : symbol -> mem * mem seq =
    ok_exn (Table.(rev_map ~one_to:at_least_one Symbol.hashable syms)) in
  let symbols_of_segment () : segment -> symbol seq =
    Table.(link ~one_to:many Segment.hashable secs syms) in
  let segment_of_symbol () : symbol -> segment =
    create_segment_of_symbol_table syms secs in
  return ({
      img; name; data; symbols = syms; segments = secs; words;
      memory_of_segment;
      memory;
      memory_of_symbol   = Lazy.from_fun memory_of_symbol;
      symbols_of_segment = Lazy.from_fun symbols_of_segment;
      segment_of_symbol  = Lazy.from_fun segment_of_symbol;
    }, errs)

let data t = t.data
let memory t = t.memory

let virtuals {img={Img.segments=(s,ss)}} =
  List.filter (s::ss) ~f:is_virtual

let load_with_doc (backend,load) data path = match load data with
  | Ok (Some doc) ->
    Bap_image_ogre.image_of_doc doc >>= fun img ->
    of_img img data path
  | Ok None ->
    Or_error.error_string @@
      sprintf "backend %s is not able to drive this file" backend
  | Error _ -> error "create image" (backend,`path path)
                 [%sexp_of:string * [`path of string option]]

let of_backend backend data path : result =
  match String.Table.find backends backend with
  | None -> errorf "no such backend: '%s'" backend
  | Some load -> load_with_doc (backend, load) data path

let autoload data path =
  let bs = String.Table.data backends in
  let run load = match load data with
    | Ok (Some doc) -> Some doc
    | _ -> None in
  match List.filter_map ~f:run bs with
  | [] -> Or_error.error_string @@
    sprintf "didn't find a suitable loader"
  | doc::docs ->
    EM.List.fold docs ~init:doc ~f:Ogre.Doc.merge >>= fun doc ->
    Bap_image_ogre.image_of_doc doc >>= fun img ->
    of_img img data path

let create_image path ?(backend="llvm") data : result =
  if backend = "auto" then autoload data path
  else of_backend backend data path

let of_bigstring ?backend data =
  create_image None ?backend data

let of_string ?backend data =
  of_bigstring ?backend (Bigstring.of_string data)

let create ?(backend="llvm") path : result =
  try_with (fun () -> Bap_fileutils.readfile path) >>= fun data ->
  if backend = "auto" then autoload data (Some path)
  else of_backend backend data (Some path)

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

(* let%test_module "image" = *)
(*   (module struct *)
(*     let expect ?(print=false) ~errors data_size ss = *)
(*       let width = 32 in *)
(*       let segment (addr, off, size) = { *)
(*         Backend.Segment.name = "test-segment"; *)
(*         Backend.Segment.perm = R; *)
(*         Backend.Segment.off; *)
(*         Backend.Segment.location = { *)
(*           Location.addr = Addr.of_int ~width addr; *)
(*           Location.len = size; *)

(*         } *)
(*       } in *)
(*       let data = Bigstring.create data_size in *)
(*       let secs = match List.map ss ~f:segment with *)
(*         | [] -> invalid_arg "empty set of segments" *)
(*         | (s::ss) -> (s,ss) in *)
(*       let v = validate_segments data secs in *)
(*       if print then begin *)
(*         match Validate.(result v) with *)
(*         | Ok () -> eprintf "No errors\n" *)
(*         | Error err -> eprintf "Errors: %s\n" @@ Error.to_string_hum err *)
(*       end; *)
(*       let has_errors = Validate.errors v <> [] in *)
(*       errors = has_errors *)

(*     let%test "" = expect ~errors:true  0 [0,0,0] *)
(*     let%test "" = expect ~errors:true  1 [0,0,0] *)
(*     let%test "" = expect ~errors:false 1 [0,0,1] *)
(*     let%test "" = expect ~errors:true  8 [0,0,4; 2,4,4] *)
(*     let%test "" = expect ~errors:true  8 [0,0,4; 4,0,4] *)
(*     let%test "" = expect ~errors:true  7 [0,0,4; 4,4,4] *)
(*     let%test "" = expect ~errors:true  8 [0,0,4; 4,1,4] *)
(*     let%test "" = expect ~errors:true  8 [0,2,4; 4,0,4] *)
(*     let%test "" = expect ~errors:true  8 [0,1,4; 4,0,4] *)
(*     let%test "" = expect ~errors:true  8 [0,0,4; 4,1,4] *)
(*   end *)
(*   ) *)
