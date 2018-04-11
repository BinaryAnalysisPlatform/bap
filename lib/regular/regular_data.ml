open Core_kernel
open Regular_data_types
open Regular_data_intf

module Read = Regular_data_read
module Write = Regular_data_write

let sexp_reader (type t) (module T : Sexpable with type t = t) =
  let of_bytes str = T.t_of_sexp (Sexp.of_string str) in
  let of_bigstring str = T.t_of_sexp (Sexp.of_bigstring str) in
  let of_channel ch = T.t_of_sexp (Sexp.input_sexp ch) in
  Read.create ~of_channel ~of_bigstring ~of_bytes ()

let sexp_writer (type t) (module T : Sexpable with type t = t) =
  let to_bytes x = Sexp.to_string (T.sexp_of_t x) in
  let size x = snd (Sexp.size (T.sexp_of_t x)) in
  let dump ch x = Sexp.output ch (T.sexp_of_t x) in
  Write.create ~size ~to_bytes ~dump ()

let bin_reader (type t) (module T : Binable with type t = t) =
  let of_bytes = Binable.of_string (module T) in
  let of_bigstring = Binable.of_bigstring (module T) in
  Read.create ~of_bytes ~of_bigstring ()

let bin_writer (type t) (module T : Binable with type t = t) =
  let to_bytes = Binable.to_string (module T) in
  let to_bigstring = Binable.to_bigstring (module T) in
  let size = T.bin_size_t in
  let blit_to_bigstring buf x pos =
    let _ : int =
      Bigstring.write_bin_prot ~pos buf T.bin_writer_t x in
    () in
  Write.create ~size ~to_bytes ~to_bigstring ~blit_to_bigstring ()

let marshal_reader (type t) (module T : T with type t = t) =
  Read.create
    ~of_channel:Marshal.from_channel
    ~of_bytes:(fun s -> Marshal.from_string s 0) ()

let marshal_writer (type t) (module T : T with type t = t) =
  let flags = Marshal.[Closures] in
  Write.create
    ~dump:(fun c x -> Marshal.to_channel c x flags)
    ~to_bytes:(fun x -> Marshal.to_bytes x flags)
    ~blit_to_string:(fun buf x off ->
        let max = Int.max_value in
        let _ : int = Marshal.to_buffer buf off max x flags in
        ())
    ()

let pretty_writer (type t)
    (module T : Pretty_printer.S with type t = t) =
  Write.create ~pp:T.pp ()

(** A functor from the minimal signature  *)
module Make(T : Versioned) = struct
  type t = T.t

  type info = string * [`Ver of string] * string option
  let version = T.version

  module Ver = Comparable.Make(struct
      type t = string [@@deriving sexp]
      let sep = '.'
      type strings = string list [@@deriving compare]
      let compare x y =
        let x = String.split ~on:sep x in
        let y = String.split ~on:sep y in
        compare_strings x y
    end)

  module Id = String

  type 'a table = 'a Ver.Map.t Id.Table.t

  type 'a with_info = {
    serializer : 'a;
    name : string;
    ver  : string;
    desc : string option;
  }

  type 'a with_desc = {
    cls : 'a;
    desc : string option;
  }


  type state = {
    mutable default_printer : t writer with_info option;
    mutable default_reader : t reader with_info;
    mutable default_writer : t writer with_info;
    writers : t writer with_desc table;
    readers : t reader with_desc table;
  }

  let set (table : 'a table) ~ver name cls : unit =
    Id.Table.change table name (function
        | None -> Some (Ver.Map.singleton ver cls)
        | Some vs -> Some (Ver.Map.add vs ver cls))

  let find_with_ver (table : 'a table) ?ver name : (string * 'a) option =
    match Hashtbl.find table name with
    | None -> None
    | Some vs -> match ver with
      | Some ver -> Option.(Map.find vs ver >>| fun x -> ver,x)
      | None -> Map.max_elt vs

  let find (table : 'a table) ?ver name : 'a option =
    Option.(find_with_ver table ?ver name >>| snd)

  let find_cls table ?ver name =
    Option.map (find table ?ver name) ~f:(fun t -> t.cls)

  let make_default serializer = {
    serializer;
    name = "marshal";
    ver = version;
    desc = Some "OCaml Standard Marshaling format"
  }

  let default_writer = make_default (marshal_writer (module T))
  let default_reader = make_default (marshal_reader (module T))

  let state = {
    default_printer = None;
    default_writer;
    default_reader;
    writers = Id.Table.create ();
    readers = Id.Table.create ();
  }

  let add_cls (tc : 'a table) ?desc ~ver name cls =
    set tc ~ver name {cls; desc}

  let add_reader = add_cls state.readers
  let add_writer = add_cls state.writers
  let find_reader = find_cls state.readers
  let find_writer = find_cls state.writers

  let available (table : 'a table) : info list =
    Hashtbl.fold table ~init:[] ~f:(fun ~key:name ~data:vers acc ->
        Map.fold vers ~init:acc ~f:(fun ~key:ver ~data:{desc} acc ->
            (name, `Ver ver, desc) :: acc))

  let get from ?ver name =
    match find from ?ver name with
    | None -> invalid_argf "Unknown class %s" name ()
    | Some d -> d.cls

  let get_with_info from ?ver name =
    match find_with_ver from ?ver name with
    | None -> invalid_argf "Unknown class %s" name ()
    | Some (ver,{cls; desc}) -> {
        ver; serializer = cls; name;
        desc;
      }

  let get_reader_with_info = get_with_info state.readers
  let get_writer_with_info = get_with_info state.writers

  let available_readers () = available state.readers
  let available_writers () = available state.writers
  let info s : info = s.name, `Ver s.ver, s.desc
  let default_printer () = Option.map state.default_printer ~f:info

  let set_default_printer ?ver name =
    state.default_printer <- Some (get_with_info state.writers ?ver name)

  let with_printer ?ver name f =
    let default = state.default_reader in
    set_default_printer ?ver name;
    protect ~f ~finally:(fun () -> state.default_reader <- default)

  let default_reader () = info state.default_reader
  let set_default_reader ?ver name =
    state.default_reader <- get_with_info state.readers ?ver name
  let with_reader ?ver name f  =
    let default = state.default_reader in
    set_default_reader ?ver name;
    protect ~f ~finally:(fun () -> state.default_reader <- default)

  let default_writer () = info state.default_writer
  let set_default_writer ?ver name =
    state.default_writer <- get_with_info state.writers ?ver name
  let with_writer ?ver name f  =
    let default = state.default_writer in
    set_default_writer ?ver name;
    protect ~f ~finally:(fun () -> state.default_writer <- default)


  let get_default ?ver ?fmt place default =
    match fmt with
    | None -> default.serializer
    | Some name -> get place ?ver name

  let writer ver fmt =
    get_default ?ver ?fmt state.writers state.default_writer

  let reader ver fmt =
    get_default ?ver ?fmt state.readers state.default_reader

  let size_in_bytes ?ver ?fmt = Write.size @@ writer ver fmt
  let of_bytes ?ver ?fmt = Read.of_bytes @@ reader ver fmt
  let to_bytes ?ver ?fmt = Write.to_bytes @@ writer ver fmt
  let blit_to_bytes ?ver ?fmt =
    Write.blit_to_string @@ writer ver fmt
  let of_bigstring ?ver ?fmt =
    Read.of_bigstring @@ reader ver fmt
  let to_bigstring ?ver ?fmt =
    Write.to_bigstring @@ writer ver fmt
  let blit_to_bigstring ?ver ?fmt =
    Write.blit_to_bigstring @@ writer ver fmt

  module Io = struct
    let load ?ver ?fmt = Read.of_channel @@ reader ver fmt

    let read ?ver ?fmt file =
      In_channel.with_file file ~f:(fun ch -> load ?ver ?fmt ch)

    let scan ?ver ?fmt ch =
      let next () =
        try Some (load ?ver ?fmt ch) with End_of_file -> None in
      next

    let load_all ?ver ?fmt ?(rev=false) ch =
      let next = scan ?ver ?fmt ch in
      let rec loop xs =
        match next () with
        | None when rev -> xs
        | None -> List.rev xs
        | Some x -> loop (x::xs) in
      loop []

    let save ?ver ?fmt = Write.to_channel @@ writer ver fmt

    let write ?ver ?fmt file data =
      Out_channel.with_file file ~f:(fun ch -> save ?ver ?fmt ch data)

    let dump ?ver ?fmt ch f =
      let rec loop () =
        match f () with
        | None -> ()
        | Some x -> save ?ver ?fmt ch x; loop () in
      loop ()

    let save_all ?ver ?fmt ch xs =
      let write = save ?ver ?fmt ch in
      let rec loop = function
        | [] -> ()
        | x :: xs -> write x; loop xs in
      loop xs

    let with_printer ?ver ?fmt f =
      match fmt with
      | Some _ -> f (writer ver fmt)
      | None -> match state.default_printer  with
        | None -> ()
        | Some writer -> f writer.serializer

    let show ?ver ?fmt x =
      with_printer ?ver ?fmt (fun writer ->
          Write.to_channel writer stdout x)

    let print ?ver ?fmt ppf x =
      with_printer ?ver ?fmt (fun writer ->
          Write.to_formatter writer ppf x)
  end

  module Cache = struct
    let cacher () =
      let reader = reader None None in
      let writer = writer None None in
      Regular_cache.Service.request reader writer
    let load id = Regular_cache.load (cacher ()) id
    let save id data = Regular_cache.save (cacher ()) id data
  end

  let () =
    let desc = "OCaml standard marshaling format" in
    let ver = version and name = "marshal" in
    add_reader ~desc ~ver name (marshal_reader (module T));
    add_writer ~desc ~ver name (marshal_writer (module T))
end
