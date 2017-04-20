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
  let readable   = "r" %: bool
  let writable   = "w" %: bool
  let executable = "x" %: bool

  let location () = scheme addr $ len
  let declare name scheme f = Ogre.declare ~name scheme f
  let named n scheme f = declare n (scheme $ name) f
  let arch    () = declare "arch" (scheme name) ident
  let segment () = declare "segment" (location ()) void_region
  let section () = declare "section" (location ()) void_region
  let code_start   () = declare "code-start" (scheme addr) ident
  let entry_point  () = declare "entry-point" (scheme addr) ident
  let value_chunk  () = declare "value-chunk" (location () $ root) region
  let named_region () = named "named-region" (location ()) region
  let named_symbol () = named "named-symbol" (scheme addr) (fun x y -> x,y)
  let rwx scheme = scheme $ readable $ writable $ executable
  let mapped () =
    declare "mapped" (location () $ off |> rwx)
      (fun addr len off r w x -> region addr len off, (r,w,x))
end

let make_perm r w x =
  [w, W; r, R; x, X] |>
  List.filter_map ~f:(fun (e, p) -> if e then Some p else None) |>
  List.reduce ~f:(fun p1 p2 -> Or (p1, p2))

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
  module Fact = Ogre.Make(M)
  open Fact.Syntax
  open Scheme

  let int_of_int64 x = match Int64.to_int x with
    | None -> Fact.failf "unable to convert int64 to int" ()
    | Some x -> Fact.return x

  let arch =
    Fact.require arch >>= fun s -> match Arch.of_string s with
    | Some s -> Fact.return s
    | None -> Fact.failf "unknown/unsupported architecture %s" s ()


  let addr_width = arch >>| Arch.addr_size >>| Size.in_bits

  let entry =
    addr_width >>= fun width ->
    Fact.require entry_point >>| Word.of_int64 ~width

  let location ~addr ~len =
    addr_width >>= fun width ->
    let addr = Word.of_int64 ~width addr in
    int_of_int64 len >>| fun len ->
    Location.Fields.create ~addr ~len

  let segments =
    addr_width >>= fun width ->
    Fact.foreach Ogre.Query.(begin
        select (from segment $ mapped $ named_region)
          ~join:[[field addr];
                 [field len ~from:segment;
                  field len ~from:named_region]]
      end) ~f:(fun {addr; len} ({data=off}, (r,w,x)) {data=name} ->
        location ~addr ~len >>= fun location ->
        int_of_int64 off >>= fun off ->
        match make_perm r w x with
        | Some perm ->
          Segment.Fields.create ~name ~off ~perm ~location |>
          Fact.return
        | None ->
          Fact.failf "can't find permissions for a \
                      segment at the address: 0x%Ld" addr ()) >>=
    Fact.Seq.all

  let sections =
    Fact.foreach Ogre.Query.(begin
        select (from section $ named_region)
          ~join:[[field addr];
                 [field len ~from:section;
                  field len ~from:named_region]]
      end) ~f:(fun {addr; len;} {data=name} ->
        location ~addr ~len >>| fun location ->
        Section.Fields.create ~name ~location) >>=
    Fact.Seq.all

  let symbol_locations start =
    Fact.foreach Ogre.Query.(begin
        select ~where:(value_chunk.(root) = int start)
          (from value_chunk)
      end) ~f:(fun {addr; len} -> location ~addr ~len) >>=
    Fact.Seq.all


  let symbol_name start =
    Fact.foreach Ogre.Query.(begin
        select ~where:(named_symbol.(addr) = int start)
          (from named_symbol)
      end) ~f:snd >>| Seq.hd

  let symbols =
    Fact.foreach Ogre.Query.(begin
        select ~join:[[field addr]]
          (from code_start)
      end) ~f:(fun addr ->
        symbol_name addr >>= function
        | None -> Fact.return None
        | Some name ->
          symbol_locations addr >>= fun locations ->
          match Sequence.to_list locations with
          | [] -> Fact.return None
          | loc::locs ->
            Symbol.Fields.create ~name
              ~is_function:true
              ~is_debug:false
              ~locations:(loc,locs) |>
            Option.some |>
            Fact.return) >>=
    Fact.Seq.all

  let image =
    arch >>= fun arch ->
    entry    >>= fun entry ->
    segments >>= fun segments ->
    sections >>= fun sections ->
    symbols  >>= fun symbols  ->
    match make_image arch entry segments sections symbols with
    | None -> Fact.failf "can't find segments in the image" ()
    | Some img -> Fact.return img
end

module Spec(M : Monad.S) = struct
  module Fact = Ogre.Make(M)
  open Fact.Syntax
  open Scheme

  let rec is p = function
    | Or (p1,p2) -> is p p1 || is p p2
    | p' -> p = p'

  let addr x = ok_exn (Word.to_int64 x)

  let location_repr {Location.addr=x; len} =
    addr x, Int64.of_int len

  let provide_segment s =
    let addr,len = location_repr @@ Segment.location s in
    let perm = Segment.perm s in
    let r,w,x = is R perm, is W perm, is X perm in
    let off = Int64.of_int (Segment.off s) in
    Fact.provide segment addr len >>= fun () ->
    Fact.provide mapped addr len off r w x >>= fun () ->
    Fact.provide named_region addr len (Segment.name s)

  let provide_section s =
    let addr, len = location_repr @@ Section.location s in
    Fact.provide section addr len >>= fun () ->
    Fact.provide named_region addr len (Section.name s)

  let provide_function s =
    let loc, locs = Symbol.locations s in
    let root,len = location_repr loc in
    Fact.provide named_symbol root (Symbol.name s) >>= fun () ->
    Fact.provide code_start root >>= fun () ->
    Fact.provide value_chunk root len root >>= fun () ->
    Fact.List.iter locs ~f:(fun loc ->
        let addr,len = location_repr loc in
        Fact.provide value_chunk addr len root)

  let provide_symbol s =
    if Symbol.is_function s then provide_function s
    else Fact.return ()

  let provide_image
      {Img.arch=a; entry; segments=(s,ss); sections; symbols} =
    Fact.provide arch (Arch.to_string a) >>= fun () ->
    Fact.provide entry_point (addr entry) >>= fun () ->
    Fact.List.iter (s::ss)  ~f:provide_segment >>= fun () ->
    Fact.List.iter sections ~f:provide_section >>= fun () ->
    Fact.List.iter symbols  ~f:provide_symbol
end

module Make (M : Monad.S) = struct
  module Image = Image(M)
  module Spec = Spec(M)
  module M = Ogre.Make(M)

  let doc_of_image img = M.exec (Spec.provide_image img) Ogre.Doc.empty
  let image_of_doc doc = M.eval Image.image doc
end
