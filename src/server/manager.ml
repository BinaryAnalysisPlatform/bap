open Core_kernel.Std
open Core_lwt.Std

open Bap.Std

module Id = struct
  type t = int64
  let of_string s : t Or_error.t =
    try Ok (Int64.of_string s) with
      exn -> Or_error.errorf "Bad ID format: '%s'" s
  include Regular.Make(struct
      include Int64
      let module_name = "Manager.Id"
    end)
end

module Ids = Id.Table

type id = Id.t
type 'a list1 = 'a * 'a list

type server = Transport.server

type 'a hashed =
  | Available of 'a Lwt.Or_error.t
  | Evicted of (unit -> 'a Lwt.Or_error.t)

type meta = {
  refs : server list1;
  arch : Arch.t;
  addr : Addr.t;
  endian : endian;
}

type 'a resource = {
  meta : meta;
  mutable data : 'a hashed;
} with fields

type context = {
  images : image resource Ids.t;
  chunks : mem   resource Ids.t;
  sections : Section.t Ids.t;
  symbols  : Symbol.t  Ids.t;
  sections_of_image  : id list Ids.t;
  symbols_of_section : id list Ids.t;
  memory_of_symbol   : id list Ids.t;
  symbol_of_memory   : id Ids.t;
  section_of_symbol  : id Ids.t;
  image_of_section   : id Ids.t;
} with fields



let t =
  let empty = Ids.create in
  {
    images = empty ();
    chunks = empty ();
    sections = empty ();
    symbols = empty ();
    sections_of_image = empty ();
    symbols_of_section = empty ();
    memory_of_symbol = empty ();
    symbol_of_memory = empty ();
    section_of_symbol = empty ();
    image_of_section = empty ();
  }


let next_id : unit -> id =
  let last = ref 0L in
  fun () ->
    let was = !last in
    Int64.incr last;
    assert Int64.(was < last.contents);
    !last

let string_of_id = Int64.to_string

let provide_memory ?file arch id mem =
  let buffer = Memory.to_buffer mem in
  let query = string_of_id id in
  let addr = Memory.min_addr mem in
  let endian = Memory.endian mem in
  let data = Available (Lwt.Or_error.return mem) in
  Transport.provide ~query ?file buffer >>|? fun refs ->
  {meta = {endian; addr; arch; refs}; data}


let add_image img =
  let img_id = next_id () in
  let file = Image.filename img in
  let arch = Image.arch img in
  let sections = Image.sections img |> Table.to_sequence in
  Lwt.Or_error.Seq.iter sections ~f:(fun (mem,sec) ->
      let sec_id = next_id () in
      Ids.add_multi t.sections_of_image ~key:img_id ~data:sec_id;
      Ids.add_exn t.image_of_section ~key:sec_id ~data:img_id;
      provide_memory arch ?file sec_id mem >>=? fun section ->
      Ids.add_exn t.chunks ~key:sec_id ~data:section;
      Ids.add_exn t.sections ~key:sec_id ~data:sec;
      let syms = Image.symbols_of_section img sec  in
      Lwt.Or_error.Seq.iter syms ~f:(fun sym ->
          let sym_id = next_id () in
          let (mem,mems) = Image.memory_of_symbol img sym in
          Ids.add_exn t.symbols ~key:sym_id ~data:sym;
          Ids.add_multi t.symbols_of_section ~key:sec_id ~data:sym_id;
          Ids.add_exn t.section_of_symbol ~key:sym_id ~data:sec_id;
          let all_mems = mem :: Seq.to_list mems in
          Lwt.Or_error.List.iter all_mems ~f:(fun mem ->
              let mem_id = next_id () in
              Ids.add_multi t.memory_of_symbol ~key:sym_id ~data:mem_id;
              Ids.add_exn t.symbol_of_memory ~key:mem_id ~data:sym_id;
              provide_memory arch ?file mem_id mem >>|? fun data ->
              Ids.add_exn t.chunks ~key:mem_id ~data) >>=? fun () ->
          if mems = Seq.empty then
            provide_memory arch ?file sym_id mem >>|? fun data ->
            Ids.add_exn t.chunks ~key:sym_id ~data
          else
            Lwt.Or_error.ok_unit)) >>=? fun () ->
  let buffer = Bigsubstring.create (Image.data img) in
  let query  = string_of_id img_id in
  Transport.provide ~query ?file buffer >>=? fun refs ->
  let addr = Image.entry_point img in
  let data = Available (Lwt.Or_error.return img) in
  let endian = Image.endian img in
  let resource = {meta={addr; arch; refs; endian}; data} in
  Ids.add_exn t.images ~key:img_id ~data:resource;
  Lwt.return (Or_error.return img_id)

let add_memory arch endian addr uri : id Lwt.Or_error.t =
  Transport.fetch uri >>=? fun data ->
  let pos = Bigsubstring.pos data in
  let len = Bigsubstring.length data in
  let data = Bigsubstring.base data in
  match Memory.create ~pos ~len endian addr data with
  | Error err ->
    Error.tag_arg err "fetching chunk from" uri Uri.sexp_of_t |>
    Lwt.Or_error.fail
  | Ok mem ->
    let id = next_id () in
    provide_memory arch id mem >>=? fun data ->
    Ids.add_exn t.chunks ~key:id ~data;
    Lwt.Or_error.return id

let string_of_id = Id.to_string


let id_of_string s =
  let open Or_error in
  Id.of_string s >>= fun id ->
  let f = Ids.mem in
  let tables = [f t.images; f t.chunks; f t.symbols; f t.sections] in
  match List.exists tables ~f:(fun f -> f id) with
  | true -> Ok id
  | false -> errorf "Id %s is not known"  s

let symbol_of_memory = Ids.find t.symbol_of_memory
let section_of_symbol = Ids.find t.section_of_symbol
let image_of_section = Ids.find t.image_of_section

let find_list tab id = match Ids.find tab id with
  | Some lst -> lst
  | None -> []

let sections_of_image = find_list t.sections_of_image
let symbols_of_section = find_list t.symbols_of_section
let memory_of_symbol = find_list t.memory_of_symbol

type nil = Nil
type sym = Symbol.t
type sec = Section.t
type img = Image.t

type ('mem, 'img, 'sec, 'sym) res = {
  mem : unit -> 'mem Lwt.Or_error.t;
  img : unit -> 'img Lwt.Or_error.t;
  sec : 'sec;
  sym : 'sym;
  res : meta;
}

type ('mem,'img,'sec,'sym,'a) visitor =
  ('mem,'img,'sec,'sym) res -> 'a Lwt.Or_error.t


(* we assume that resource references are sorted in the order of their
   availability, i.e. at the beginining of the list we have most fast
   and robust sources, while at the end we have last resorts.
*)
let try_all (s,ss) =
  let open Transport in
  let open Lwt in
  fetch s.uri >>= fun data ->
  List.fold ss ~init:data ~f:(fun fetched next_src -> match fetched with
      | Ok data -> return (Ok data)
      | Error err -> fetch next_src.uri >>|
        Result.map_error ~f:(fun err' -> Error.of_list [err;err']))

let fetch resource = match resource.data with
  | Available r -> r
  | Evicted load ->
    let r = load () in
    resource.data <- Available r;
    r

let print_warnings warnings = Lwt.return_unit

let image_of_bigsubstring ?backend substr =
  let base = Bigsubstring.base substr in
  if Bigsubstring.length substr <> Bigstring.length base ||
     Bigsubstring.pos substr <> 0 then
    Or_error.errorf "Unsupported: creating images from substring"
  else Image.of_bigstring ?backend base

let create_image ?backend data =
  let open Lwt in
  match image_of_bigsubstring ?backend data with
  | Ok (img,warns) -> print_warnings warns >>= fun () -> return (Ok img)
  | Error err -> return (Error err)


let add_file ?backend uri =
  Transport.fetch uri >>=? create_image ?backend  >>=? add_image

let nothing () = Lwt.Or_error.return Nil

let init r = {
  mem = nothing;
  img = nothing;
  sec = Nil;
  sym = Nil;
  res = r.meta;
}

let find_in field id : 'a Or_error.t =
  match Ids.find (Fieldslib.Field.get field t) id with
  | Some x -> Ok x
  | None ->
    Or_error.errorf "Failed to find id %a in table %s"
      Id.str id (Fieldslib.Field.name field)


let of_image res : (mem list1,img,nil,nil) res =
  let img () = fetch res in
  let mem () = img () >>=? fun img ->
    Table.to_sequence (Image.sections img)  |>
    Sequence.map ~f:fst |> Sequence.to_list |>
    function [] -> Lwt.Or_error.errorf "empty image"
           | s::ss -> Lwt.Or_error.return (s,ss) in
  { (init res) with img; mem}

let of_section sec id : (mem,img,sec,nil) res Or_error.t =
  let open Or_error in
  let open Fields_of_context in
  find_in image_of_section id >>= fun img_id ->
  find_in images img_id >>= fun img ->
  find_in chunks id >>= fun mem ->
  let res = init img in
  let img () = fetch img in
  let mem () = fetch mem in
  return { res with img; mem; sec}

let of_symbol sym id : (mem list1,img,sec,sym) res Or_error.t =
  let open Fields_of_context in
  let open Or_error in
  find_in section_of_symbol id    >>= fun sec_id ->
  find_in sections sec_id         >>= fun sec    ->
  find_in image_of_section sec_id >>= fun img_id ->
  find_in images img_id           >>= fun img ->
  find_in memory_of_symbol id     >>= fun mems ->
  List.map mems ~f:(find_in chunks) |> all >>= function
  | [] -> errorf "Symbol without memory"
  | m::ms ->
    let res = init img in
    let img () = fetch img in
    let mem () =
      fetch m >>=? fun m ->
      Lwt.Or_error.List.map ~how:`Parallel ms ~f:fetch >>=? fun ms ->
      Lwt.Or_error.return (m,ms) in
    return { res with img; sym; sec; mem}

module Return = struct
  let unit res = Lwt.Or_error.ok_unit
  let none res = Lwt.Or_error.return None
  let null res = Lwt.Or_error.return 0
  let nil  res = Lwt.Or_error.ok_nil

  let errorf fmt =
    Printf.ksprintf (fun msg -> fun _ -> Lwt.Or_error.error_string msg) fmt

  let error msg data sexp _res = Lwt.Or_error.error msg data sexp
end


let fetch_memory r = r.mem ()
let fetch_image  r = r.img ()
let section r = r.sec
let symbol  r = r.sym
let endian  r = r.res.endian
let addr    r = r.res.addr
let arch    r = r.res.arch
let refs    r = r.res.refs

let with_resource ~chunk ~symbol ~section ~image (id : id) =
  let open Fields_of_context in
  match Ids.find t.images id with
  | Some img -> image (of_image img)
  | None -> match Ids.find t.sections id with
    | Some sec -> return (of_section sec id) >>=? section
    | None -> match Ids.find t.symbols id with
      | Some sym -> return (of_symbol sym id) >>=? symbol
      | None -> match Ids.find t.chunks id with
        | Some mem -> chunk {(init mem) with mem = fun () -> fetch mem}
        | None -> Lwt.Or_error.errorf "unknown id: %a" Id.str id


let list_of_list1 (x,xs) = x::xs

let servers_of_id id =
  match Ids.find t.images id with
  | Some {meta = {refs = (x,xs)}} -> x::xs
  | None -> match Ids.find t.chunks id with
    | Some {meta = {refs = (x,xs)}} -> x::xs
    | None -> []

let links_of_id id =
  servers_of_id id |>
  List.map ~f:(fun s -> Transport.(s.uri))
