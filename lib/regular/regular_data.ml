open Core_kernel
open Regular_data_types
open Regular_data_intf

module Read = Regular_data_read
module Write = Regular_data_write

type info = string * [`Ver of string] * string option

let sexp_reader (type t) (module T : Sexpable with type t = t) =
  let of_bytes str = T.t_of_sexp (Sexp.of_string (Bytes.to_string str)) in
  let of_bigstring str = T.t_of_sexp (Sexp.of_bigstring str) in
  let of_channel ch = T.t_of_sexp (Sexp.input_sexp ch) in
  Read.create ~of_channel ~of_bigstring ~of_bytes ()

let sexp_writer (type t) (module T : Sexpable with type t = t) =
  let to_bytes x = Bytes.of_string @@ Sexp.to_string (T.sexp_of_t x) in
  let size x = snd (Sexp.size (T.sexp_of_t x)) in
  let dump ch x = Sexp.output ch (T.sexp_of_t x) in
  Write.create ~size ~to_bytes ~dump ()

let bin_reader (type t) (module T : Binable with type t = t) =
  let of_bytes bs = Binable.of_string (module T) (Bytes.to_string bs) in
  let of_bigstring = Binable.of_bigstring (module T) in
  Read.create ~of_bytes ~of_bigstring ()

let bin_writer (type t) (module T : Binable with type t = t) =
  let to_bytes x = Binable.to_string (module T) x |> Bytes.of_string in
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
    ~of_bytes:(fun s -> Marshal.from_bytes s 0) ()

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

type 'a table = 'a Ver.Map.t Id.Map.t

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


type 'a definition = {
  module_name : string option;
  default_printer : 'a writer with_info option;
  default_reader : 'a reader with_info;
  default_writer : 'a writer with_info;
  writers : 'a writer with_desc table;
  readers : 'a reader with_desc table;
}



module Class = struct
  type 'a key = {
    key : 'a Type_equal.Id.t;
  }

  type 'a t = 'a key

  module Key = struct
    type 'a t = 'a key
    let to_type_id {key} = key
    let sexp_of_t _ {key} = Sexp.Atom (Type_equal.Id.name key)
  end

  module Registry = Univ_map.Make(Key)(struct
      type 'a t = 'a definition
      let sexp_of_t _ = sexp_of_opaque
    end)

  let registry = ref Registry.empty

  let declare ?(name="opaque") () =
    {key = Type_equal.Id.create ~name sexp_of_opaque}

  let define key data =
    registry := Registry.add_exn !registry key data

  let refine cls ~f =
    registry := Registry.update !registry cls ~f:(function
        | None -> invalid_argf "Data class %s is not defined yet"
                    (Type_equal.Id.name cls.key) ()
        | Some x -> f x)


  let update (get,set) cls ~f =
    refine cls ~f:(fun meta ->
        set meta (f (get meta)))

  let add_method field cls ?desc ~ver name met : unit =
    update field cls ~f:(fun mets -> Map.change mets name ~f:(function
        | None -> Some (Ver.Map.singleton ver {cls=met; desc})
        | Some vs -> Some (Ver.Map.set vs ver {cls=met; desc})))


  let find_with_ver (table : 'a table) ?ver name : (string * 'a) option =
    match Map.find table name with
    | None -> None
    | Some vs -> match ver with
      | Some ver -> Option.(Map.find vs ver >>| fun x -> ver,x)
      | None -> Map.max_elt vs

  let find_with_info from ?ver name =
    match find_with_ver from ?ver name with
    | None -> invalid_argf "Unknown class %s" name ()
    | Some (ver,{cls; desc}) -> {
        ver; serializer = cls; name;
        desc;
      }

  let find (table : 'a table) ?ver name : 'a option =
    Option.(find_with_ver table ?ver name >>| snd)

  let find_info (table : 'a table) ?ver name =
    Option.(find_with_info table ?ver name)

  let find_cls table ?ver name =
    Option.map (find table ?ver name) ~f:(fun t -> t.cls)

  let get (get,_) key =
    get (Registry.find_exn !registry key)

  let get_method fld cls ?ver name =
    find_cls (get fld cls) ?ver name

  let get_method_with_info fld cls ?ver name =
    find_info (get fld cls) ?ver name

  module Fields = struct
    let writer_field =
      (fun {writers} -> writers),
      (fun s writers -> {s with writers})

    let reader_field =
      (fun {readers} -> readers),
      (fun s readers -> {s with readers})

    let default_printer_field =
      (fun {default_printer} -> default_printer),
      (fun s default_printer -> {s with default_printer})

    let default_reader_field =
      (fun {default_reader} -> default_reader),
      (fun s default_reader -> {s with default_reader})

    let default_writer_field =
      (fun {default_writer} -> default_writer),
      (fun s default_writer -> {s with default_writer})


    let module_name_field =
      (fun {module_name} -> module_name),
      (fun s module_name -> {s with module_name})
  end


  open Fields

  let writers cls = get writer_field cls
  let readers cls = get reader_field cls

  let collect_info (get,_) def =
    let name = match def.module_name with
      | None -> "unknown"
      | Some name -> name in
    name,
    Map.fold (get def) ~init:[] ~f:(fun ~key:name ~data:vers acc ->
        Map.fold vers ~init:acc ~f:(fun ~key:ver ~data:{desc} acc ->
            (name, `Ver ver, desc) :: acc))

  type collector = {
    collect : 'a. 'a definition -> string * info list
  }

  let writers_collector = {
    collect = fun def -> collect_info writer_field def
  }
  let readers_collector = {
    collect = fun def -> collect_info reader_field def
  }

  let info {collect}  =
    Registry.to_alist !registry |>
    List.map ~f:(fun (Registry.Packed.T (_,data)) ->
        collect data)

  let all_readers () = info readers_collector
  let all_writers () = info writers_collector
end

open Class.Fields
type 'a cls = 'a Class.t

module type Instance = sig
  include Versioned
  val instance : t cls
end

module type With_instance = sig
  include Instance
  include Data with type t := t
end

module New(T : Versioned.S) : Instance with type t = T.t = struct
  include T
  let instance : t cls = Class.declare ()
  type nonrec info = info
  let version = T.version

  let cls = instance

  let make_default serializer = {
    serializer;
    name = "marshal";
    ver = version;
    desc = Some "OCaml Standard Marshaling format"
  }

  let default_writer = make_default (marshal_writer (module T))
  let default_reader = make_default (marshal_reader (module T))


  let () = Class.define cls {
      module_name = None;
      default_printer = None;
      default_writer;
      default_reader;
      writers = Id.Map.empty;
      readers = Id.Map.empty;
    }
end

(** A functor from the minimal signature  *)
module Extend(T : sig
    include Versioned.S
    val instance : t cls
  end) : With_instance with type t := T.t = struct
  type t = T.t

  type nonrec info = info
  let version = T.version
  let instance = T.instance
  let cls = T.instance

  let add_reader = Class.add_method reader_field cls
  let add_writer = Class.add_method writer_field cls
  let find_reader = Class.get_method reader_field cls
  let find_writer = Class.get_method writer_field cls

  let enum_info (table : 'a table) : info list =
    Map.fold table ~init:[] ~f:(fun ~key:name ~data:vers acc ->
        Map.fold vers ~init:acc ~f:(fun ~key:ver ~data:{desc} acc ->
            (name, `Ver ver, desc) :: acc))

  let available_readers () = enum_info (Class.readers cls)
  let available_writers () = enum_info (Class.writers cls)
  let info s : info = s.name, `Ver s.ver, s.desc

  let set_default_printer ?ver name =
    Class.update default_printer_field cls  ~f:(fun _ ->
        Some (Class.get_method_with_info writer_field cls ?ver name))

  let with_printer ?ver name f =
    let default = Class.get default_writer_field cls in
    set_default_printer ?ver name;
    protect ~f ~finally:(fun () ->
        Class.update default_writer_field cls ~f:(fun _ -> default))


  let default_printer () =
    Class.get default_printer_field cls |>
    Option.map ~f:info

  let default_reader () =
    info @@ Class.get default_reader_field cls

  let set_default_reader ?ver name =
    Class.update default_reader_field cls ~f:(fun _ ->
        Class.get_method_with_info reader_field cls ?ver name)

  let with_reader ?ver name f  =
    let default = Class.get default_reader_field cls in
    set_default_reader ?ver name;
    protect ~f ~finally:(fun () ->
        Class.update default_reader_field cls ~f:(fun _ -> default))

  let default_writer () =
    info @@ Class.get default_writer_field cls

  let set_default_writer ?ver name =
    Class.update default_writer_field cls ~f:(fun _ ->
        Class.get_method_with_info writer_field cls ?ver name)

  let with_writer ?ver name f  =
    let default = Class.get default_writer_field cls in
    set_default_writer ?ver name;
    protect ~f ~finally:(fun () ->
        Class.update default_writer_field cls ~f:(fun _ -> default))

  let get_default ?ver ?fmt field default_field =
    match fmt with
    | None -> (Class.get default_field cls).serializer
    | Some name ->
      (Class.get_method_with_info field cls ?ver name).serializer

  let writer ver fmt =
    get_default ?ver ?fmt writer_field default_writer_field

  let reader ver fmt =
    get_default ?ver ?fmt reader_field default_reader_field

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
      | None -> match Class.get default_printer_field cls with
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

module Make(T : Versioned) = struct
  module Self = New(T)
  include Self
  include Extend(Self)
end

let all_writers = Class.all_writers
let all_readers = Class.all_readers

let set_module_name cls name = Class.update module_name_field cls ~f:(fun _ ->
    Some name)
