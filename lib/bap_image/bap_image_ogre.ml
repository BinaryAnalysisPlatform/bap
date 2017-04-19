open Core_kernel.Std
open Bap_types.Std
open Monads.Std

open Image_backend

module Location = Image_common.Location

type img = Img.t
type doc = Ogre.doc

module Scheme = struct
  open Ogre.Type

  type 'a region = {addr : int64; len: int64; data : 'a}
  let region addr len data = {addr; len; data}
  let void_region addr len = {addr; len; data = ()}

  let len  = "len"  %: int
  let off  = "off"  %: int
  let addr = "addr"  %: int
  let name = "name"  %: str
  let root = "root"  %: int
  let is_set = "set" %: bool

  let location () = scheme addr $ len
  let declare name scheme f = Ogre.declare ~name scheme f
  let flag name = declare name (location () $ is_set) region
  let named n scheme f = declare n (scheme $ name) f

  let arch    () = declare "arch" (scheme name) ident
  let mapped  () = declare "mapped" (location () $ off) region
  let segment () = declare "segment" (location ()) void_region
  let section () = declare "section" (location ()) void_region
  let code_start   () = declare "code-start" (scheme addr) ident
  let entry_point  () = declare "entry-point" (scheme addr) ident
  let value_chunk  () = declare "value-chunk" (location () $ root) region
  let named_region () = named "named-region" (location ()) region
  let named_symbol () = named "named-symbol" (scheme addr) Tuple.T2.create
  let executable () = flag "executable"
  let writable () = flag "writable"
  let readable () = flag "readable"
end

let make_perm w x =
  [w, W; true, R; x, X] |>
  List.filter_map ~f:(fun (e, p) -> if e then Some p else None) |>
  List.reduce ~f:(fun p1 p2 -> Or (p1, p2)) |>
  Option.value_exn

let make_image arch entry segments sections symbols =
  match Seq.to_list segments with
  | [] -> None
  | s::seg ->
    let segments = s, seg in
    let sections = Seq.to_list sections in
    let symbols = Seq.to_list symbols |> List.filter_map ~f:ident in
    Option.some @@
    Img.Fields.create ~arch ~entry ~segments ~sections ~symbols

module Image(M : Monad.S) = struct
  module Query = Ogre.Query
  module M = Ogre.Make(M)
  open Scheme
  open M

  let word_of_int64 width = Word.of_int64 ~width

  let int_of_int64 x = match Int64.to_int x with
    | None -> failf "unable to convert int64 to int" ()
    | Some x -> return x

  let select_foreach ?join ?where ~f from =
    foreach (Query.select ?join ?where from) ~f

  let arch () =
    require arch >>= fun s -> match Arch.of_string s with
    | Some s -> return s
    | None -> failf "unknown arch %s" s ()

  let entry w =
    require entry_point >>= fun e ->
    return (word_of_int64 w e)

  let location width addr len =
    let addr = word_of_int64 width addr in
    int_of_int64 len >>= fun len ->
    return (Location.Fields.create ~addr ~len)

  let segments width =
    foreach
      Query.(begin
          select
            (from segment $ mapped $ named_region $ writable $ executable)
            ~join:[[field addr];
                   [field len ~from:segment;
                    field len ~from:named_region]]
        end)
      ~f:(fun {addr;len} {data=off} {data=name} {data=w} {data=x} ->
          location width addr len >>= fun location ->
          int_of_int64 off >>= fun off ->
          let perm = make_perm w x in
          return (Segment.Fields.create ~name ~off ~perm ~location))

  let sections w =
    foreach
      Query.(select (from section))
      ~f:(fun {addr=address; len=length} ->
          foreach Query.(
              select (from named_region)
                ~where:(named_region.(addr) = int address &&
                        named_region.(len) = int length))
            ~f:(fun {data=name} ->
                location w address length >>= fun location ->
                return (Section.Fields.create ~name ~location))) >>=
    fun s -> return (Sequence.concat s)

  let sections' w =
    foreach
      Query.(begin
          select (from section $ named_region)
            ~join:[[field addr];
                   [field len ~from:section;
                    field len ~from:named_region]]
        end)
      ~f:(fun {addr; len;} {data=name} ->
          location w addr len >>= fun location ->
          return (Section.Fields.create ~name ~location))

  let symbol_locations width root_addr =
    foreach
      Query.(select (from value_chunk)
               ~where:(value_chunk.(root) = int root_addr))
      ~f:(fun {addr; len} -> location width addr len)

  let symbol_name address =
    foreach
      Query.(select (from named_symbol)
               ~where:(named_symbol.(addr) = int address))
      ~f:(fun (_,name) -> return name) >>= fun names ->
    match Sequence.to_list names with
    | [] -> return ""
    | name :: _ -> return name

  let symbols w =
    foreach
      Query.(select (from code_start))
      ~f:(fun addr ->
          symbol_name addr >>= fun name ->
          symbol_locations w addr >>= fun locations ->
          match Sequence.to_list locations with
          | [] -> return None
          | loc::locs ->
            let locations = loc,locs in
            return @@ Option.some @@
            Symbol.Fields.create ~name
              ~is_function:true ~is_debug:false ~locations)

  let image () =
    arch () >>= fun arch ->
    let w = match Arch.addr_size arch with
      | `r32 -> 32
      | `r64 -> 64 in
    entry    w >>= fun entry ->
    segments w >>= fun segments ->
    sections w >>= fun sections ->
    symbols  w >>= fun symbols  ->
    match make_image arch entry segments sections symbols with
    | None -> failf "segments list is empty" ()
    | Some img -> return img
end

module Spec(M : Monad.S) = struct
  module M = Ogre.Make(M)
  open M
  open Scheme

  let rec checked p = function
    | Or (p1,p2) -> checked p p1 || checked p p2
    | X|W|R as p' -> p = p'

  let int64_of_word word =
    match Word.to_int64 word with
    | Ok word -> return word
    | Error er -> fail er

  let arch a = provide arch (Sexp.to_string @@ Arch.sexp_of_t a)
  let entry e = int64_of_word e >>= fun e -> provide entry_point e

  let segment s =
    let loc = Segment.location s in
    let addr, len = Location.(addr loc, len loc) in
    let len = Int64.of_int len in
    let perm = Segment.perm s in
    int64_of_word addr >>= fun addr ->
    provide segment addr len >>= fun () ->
    provide mapped addr len (Int64.of_int @@ Segment.off s) >>= fun () ->
    provide named_region addr len (Segment.name s) >>= fun () ->
    provide writable addr len (checked W perm) >>= fun () ->
    provide readable addr len (checked R perm) >>= fun () ->
    provide executable addr len (checked X perm)

  let section s =
    let loc = Section.location s in
    let addr, len = Location.(addr loc, len loc) in
    let len = Int64.of_int len in
    int64_of_word addr >>= fun addr ->
    provide section addr len >>= fun () ->
    provide named_region addr len (Section.name s)

  let code name loc =
    let len = Int64.of_int @@ Location.len loc in
    int64_of_word (Location.addr loc) >>= fun addr ->
    provide named_symbol addr name >>= fun () ->
    provide value_chunk addr len addr >>= fun () ->
    provide code_start addr

  let symbol s =
    let loc, locs = Symbol.locations s in
    if Symbol.is_function s then
      List.iter ~f:(code (Symbol.name s)) (loc::locs)
    else return ()

  let of_image img =
    arch  (Img.arch  img) >>= fun () ->
    entry (Img.entry img) >>= fun () ->
    let seg, segs = Img.segments img in
    List.iter (seg::segs) ~f:segment >>= fun () ->
    List.iter (Img.sections img) ~f:section >>= fun () ->
    List.iter (Img.symbols img) ~f:symbol
end

module Make (M : Monad.S) = struct
  module Image = Image(M)
  module Spec = Spec(M)
  module M = Ogre.Make(M)

  let doc_of_image img = M.exec (Spec.of_image img) Ogre.Doc.empty
  let image_of_doc doc = M.eval (Image.image ()) doc
end
