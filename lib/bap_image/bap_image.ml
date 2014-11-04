open Core_kernel.Std
open Bap_types.Std
open Image_types
open Or_error

module Seq = Sequence

module type ErrorMonad = sig
  include Monad.S
  val errorf: ('a,unit,string,_ t) format4 -> 'a
  val validate_result: Validate.t -> unit t
end

module ErrorNullMonad = struct
  module Basic = struct
    type 'a t = 'a
    let bind m f = f m
    let map m ~f = f m
    let return = ident
    let map = `Custom map
  end
  include Basic
  include Monad.Make(Basic)

  let errorf format = Printf.ksprintf failwith format
  let validate_result v = match Validate.result v with
    | Ok () -> return ()
    | Error err -> errorf "Validation failed: %s" @@
      Error.to_string_hum err
end

module MakeMem(M : ErrorMonad) = struct
  open M
  type 'a m = 'a M.t
  type 'a seq = 'a Sequence.t

  type getter = pos_ref : addr ref -> word m

  module Getters = struct
    type getters = {
      int8  : getter;
      uint8 : getter;
      int16 : getter;
      uint16: getter;
      int32 : getter;
      int64 : getter;
    } with fields
  end

  open Getters

  type region = {
    off  : int;
    size : int;
    data : Bigstring.t;
    addr : addr;
    get  : getters;
    get_word : getter;
  }

  type t = {
    addr_size : Word_size.t;
    endian : endian;
    secs: region array;
  }

  let (>>|) = (>>|)
  let (>>=) = (>>=)

  let min_addr t = t.secs.(0).addr


  let max_addr t =
    let i = Array.length t.secs - 1 in
    let n = t.secs.(i).size - 1 in
    Addr.(t.secs.(i).addr ++ n)

  let with_error : 'a Or_error.t -> 'a m = function
    | Ok v -> return v
    | Error err -> errorf "%s" @@ Error.to_string_hum err

  (** [creat_getters endian addr offset size data] creates a getters
      class according to a specified paramters. All parameters will be
      encapsulated inside a closure (cf, [getter] type).
      [addr] is a virtual address of the byte located at the [offset]
      from the beginning of the [data] *)
  let create_getters addr_size endian addr off size data  =
    let length = Bigstring.length data in
    (* this precondition should be checked before entry *)
    assert (off + size <= length);
    let make inj bytes read ~pos_ref =
      let m = Addr.Int.of_word_size addr_size in
      with_error Addr.Int.(m !pos_ref - m addr) >>= fun addr ->
      with_error Addr.(to_int (addr ++ off)) >>= fun pos ->
      if pos < off then
        errorf "segfault: addr < min_addr" else
      if pos >= off + size then
        errorf "segfault: addr > max_addr" else
      if pos + bytes > off + size then
        errorf "segfault: addr + %d > max_addr\n" bytes
      else
        let r = read data ~pos in
        pos_ref := Addr.(!pos_ref ++ bytes);
        return (inj r) in
    let int n = make (Word.of_int ~width:(n*8)) n in
    let int32 = make Word.of_int32 in
    let int64 = make Word.of_int64 in
    let open Bigstring in
    let get =
      if endian = BigEndian then {
        int8   = int   1 unsafe_get_int8;
        uint8  = int   1 unsafe_get_uint8;
        int16  = int   2 unsafe_get_int16_be;
        uint16 = int   2 unsafe_get_uint16_be;
        int32  = int32 4 unsafe_get_int32_t_be;
        int64  = int64 8 unsafe_get_int64_t_be;
      } else {
        int8   = int 1 unsafe_get_int8;
        uint8  = int 1 unsafe_get_uint8;
        int16  = int 2 unsafe_get_int16_le;
        uint16 = int 2 unsafe_get_uint16_le;
        int32  = int32 4 unsafe_get_int32_t_le;
        int64  = int64 8 unsafe_get_int64_t_le;
      } in
    let get_word = match addr_size with
      | Word_size.W64 -> get.int64
      | Word_size.W32 -> get.int32 in
    get,get_word

  let validate_no_intersections secs : Validate.t =
    let open Backend.Section in
    match Array.to_list secs with
    | [] -> Validate.fail "empty image"
    | x :: xs ->
      let (vs,_) = List.fold xs ~init:([],x) ~f:(fun (vs,s1) s2 ->
          let v =
            Addr.(validate_lbound
                    ~min:(Incl (s1.addr ++ s1.size)) s2.addr) in
          (v::vs, s2)) in
      Validate.name_list "sections intersection" vs

  let validate_section sec : Validate.t =
    let module BS = Backend.Section in
    let max_size = Bigstring.length BS.(sec.data) in
    let passed _ = Validate.pass in
    let assume what = Validate.field_folder sec what in
    Validate.name_list "section" begin
      BS.Fields.fold
        ~init:[]
        ~name:(assume passed)
        ~addr:(assume Addr.validate_non_negative)
        ~perm:(assume passed)
        ~off: (assume @@ Int.validate_bound
                 ~min:(Incl 0)
                 ~max:(Excl max_size))
        ~size:(assume @@ Int.validate_bound
                 ~min:(Excl 0)
                 ~max:(Incl (max_size - BS.off sec)))
        ~data:(assume passed)
    end

  let validate_sections secs : Validate.t =
    let check_secs = Validate.list_indexed validate_section in
    Validate.of_list [
      check_secs (Array.to_list secs);
      validate_no_intersections secs
    ]

  (** [of_sections addr_size endian sections] creates a memory image
      based on sections, that was provided by a backend.  *)
  let of_sections addr_size endian secs =
    let module BS = Backend.Section in
    let secs = Array.sorted_copy secs
        ~cmp:(fun s1 s2 -> Addr.compare s1.BS.addr s2.BS.addr) in
    validate_result (validate_sections secs) >>| fun () ->
    let secs = Array.map secs ~f:(fun s ->
        let addr, off, size, data =
          s.BS.addr, s.BS.off, s.BS.size, s.BS.data in
        let get,get_word = create_getters addr_size endian addr off size data in
        { get; get_word; off; size; data; addr}) in
    {addr_size; endian; secs}

  let word_of_size reg = function
    | None    -> return reg.get_word
    | Some 8  -> return reg.get.uint8
    | Some 16 -> return reg.get.uint16
    | Some 32 -> return reg.get.int32
    | Some 64 -> return reg.get.int64
    | Some n  -> errorf "unsupported word size %d" n

  let contains mem =
    Addr.between ~low:(min_addr mem) ~high:(max_addr mem)

  let contains_reg reg = Addr.between
      ~low:(reg.addr)
      ~high:Addr.(reg.addr ++ (reg.size - 1))

  let find_region t addr =
    match Array.findi  t.secs (fun i reg -> contains_reg reg addr) with
    | Some (i,reg) -> return (i,reg)
    | None ->
      errorf "address %s is not mapped into memory" @@
      Addr.to_string addr

  let input_word ?word_size t ~pos_ref  =
    find_region t !pos_ref >>= fun (_,sec) ->
    word_of_size sec word_size >>= fun read ->
    read ~pos_ref

  let get ?word_size t addr : word m =
    input_word t ?word_size ~pos_ref:(ref addr)


  module Input = struct
    type 'a reader = t -> pos_ref : addr ref -> 'a m

    let word = input_word

    let read  get t ~pos_ref =
      find_region t !pos_ref >>= fun (_,sec) ->
      get sec.get ~pos_ref

    let int8 = read int8
    let uint8 = read uint8
    let int16 = read int16
    let uint16 = read uint16
    let int32 = read int32
    let int64 = read int64
  end

  let sec_foldi ?word_size sec ~(init:'a) ~f  =
    word_of_size sec word_size >>= fun read ->
    let pos_ref = ref sec.addr in
    let finish = Addr.(sec.addr ++ sec.size) in
    let rec loop init =
      if Addr.(pos_ref.contents < finish)
      then
        let addr = !pos_ref in
        read ~pos_ref >>= fun word ->
        f addr init word >>= loop
      else
        return init in
    loop init

  let foldi ?word_size t ~(init : 'a) ~f : 'a m =
    let n = Array.length t.secs in
    let rec loop init i =
      if i < n
      then
        sec_foldi ?word_size t.secs.(i) ~init ~f >>= fun init ->
        loop init (i+1)
      else
        return init in
    loop init 0

  let iteri ?word_size t ~f : unit m =
    foldi t ?word_size ~init:() ~f:(fun i () w -> f i w)

  let fold ?word_size t ~(init:'a) ~f : 'a m =
    foldi t ?word_size ~init ~f:(fun (_ : addr) init word -> f init word)

  let iter ?word_size t ~f : unit m =
    iteri t ?word_size ~f:(fun (_ : addr) word -> f word)

  let exists ?word_size t ~f : bool m =
    let run r =
      iter t ?word_size ~f:(fun word -> f word >>= function
        | true -> r.return (return true)
        | false -> return ()) >>= fun () -> return true in
    with_return run

  let for_all ?word_size t ~f : bool m =
    let run r =
      iter t ?word_size ~f:(fun word -> f word >>= function
        | false -> r.return (return false)
        | true -> return ()) >>= fun () -> return false in
    with_return run

  let mem ?word_size t x : bool m =
    exists t ?word_size ~f:(fun y -> return Word.(x = y))

  let count ?word_size t ~f : int m =
    fold t ?word_size ~init:0 ~f:(fun n x -> f x >>| function
      | true -> n + 1
      | false -> n)

  let find ?word_size t ~f : word option m =
    let run r =
      iter t ?word_size ~f:(fun w -> f w >>= function
        | true -> r.return (return (Some w))
        | false -> return ()) >>= fun () -> return None in
    with_return run

  let find_map ?word_size t ~f : 'a option m =
    let run r =
      iter t ?word_size ~f:(fun w -> f w >>= function
        | None -> return ()
        | some -> r.return (return some)) >>= fun () -> return None in
    with_return run

  let to_list ?word_size t  : word list m =
    fold t ?word_size ~init:[] ~f:(fun xs x -> return (x::xs)) >>| List.rev

  let size t = function
    | Some (8|16|32|64 as n) -> return (n / 8)
    | Some n -> errorf "unsupported word size: %d" n
    | None -> return (Word_size.num_bits t.addr_size / 8)


  let length ?word_size t : int m =
    size t word_size >>= fun word_size ->
    let bytes =
      Array.fold t.secs ~init:0 ~f:(fun sum sec -> sum + sec.size) in
    let len = bytes / word_size in
    if len * word_size = bytes then return len
    else errorf "length: there an is unwhole number of words \
                 for the specifed word size"

  let to_array ?word_size t : word array m =
    length ?word_size t >>= fun len ->
    size t word_size >>= fun n ->
    let xs = Array.create ~len Word.(of_int ~width:(n*8) 0) in
    fold ?word_size t ~init:0 ~f:(fun i x -> xs.(i) <- x; return (i+1))
    >>= fun (_ : int) -> return xs


  let sub copy ?word_size ?from ?(words : int option) t : t m =
    let amin = Option.value from ~default:(min_addr t) in
    size t word_size  >>= fun word_size ->
    find_region t amin >>= fun (imin,_) ->
    let amax =
      match Option.map words ~f:(fun w -> w * word_size) with
      | None -> max_addr t
      | Some size -> Addr.(amin ++ (size - 1)) in
    with_error
      Addr.Int.(!$amax - !$amin >>= Addr.to_int) >>= fun diff ->
    let size = diff + 1 in
    find_region t amax >>= fun (imax,_) ->
    let snum = imax - imin + 1 in
    List.range 0 snum |> List.fold ~init:(return (0,[]))
      ~f:(fun state j ->
          state >>= fun (bytes,acc) ->
          let i = j + imin in
          let s = t.secs.(i) in
          let addr = Addr.max amin s.addr in
          with_error Addr.Int.(!$addr - !$(s.addr) >>= Addr.to_int)
          >>= fun off ->
          let off = s.off + off in
          let size = Int.min (size - bytes) (s.size - off) in
          let data = s.data in
          let get,get_word =
            create_getters t.addr_size t.endian
              addr off size data in
          let reg = {off; size; data; addr; get; get_word} in
          return (bytes + size, reg :: acc))
    >>= fun (_,regs) ->
    let regs = List.rev regs in
    return { t with secs = Array.of_list regs}

  let view = sub ident
  let copy = sub Bigstring.subo

  let try_option str = function
    | Some s -> return s
    | None -> errorf str

  let hexdump t fmt =
    let print_char c =
      let c = match Char.of_int c with
        | Some c when Char.is_print c -> c
        | _ -> '.' in
      Format.fprintf fmt "%c" c in

    let print_chars off = function
      | [] -> ()
      | chars ->
        Format.fprintf fmt "%*s" (3*off + 1) "|";
        List.iter (List.rev chars) ~f:print_char;
        Format.fprintf fmt "%*s\n" (off + 1) "|" in
    foldi t ~word_size:8 ~init:[] ~f:(fun addr chars char ->
        let newline = chars = [] || List.length chars = 16 in
        with_error Addr.(to_int addr) >>= fun addr ->
        with_error Word.(to_int char) >>= fun char ->
        size t None >>= fun addr_len ->
        if newline then begin
          print_chars 0 chars;
          Format.fprintf fmt "%0*X  " (addr_len * 2) addr;
        end;
        Format.fprintf fmt "%02X " char;
        return (if newline then [char] else char :: chars))
    >>= fun chars ->
    let x = 16 - List.length chars in
    print_chars x chars;
    return ()
end

module Or_error_with_validate = struct
  include Or_error
  let validate_result = Validate.result
end

module Mem = MakeMem(Or_error_with_validate)
module Mem_exn = MakeMem(ErrorNullMonad)

(* maybe a cleaner solution, would be to lift memory type outside
   the functor and to make it polymorphic over monad type. Maybe not *)
let memory_with_exn m = Mem.({
    Mem_exn.addr_size = m.addr_size;
    Mem_exn.endian = m.endian;
    Mem_exn.secs = Array.map m.secs ~f:(fun sec ->
        let get, get_word =
          Mem_exn.create_getters m.addr_size m.endian
            sec.addr sec.off sec.size sec.data in
        {
          Mem_exn.off = sec.off;
          Mem_exn.size = sec.size;
          Mem_exn.data = sec.data;
          Mem_exn.addr = sec.addr;
          Mem_exn.get = get;
          Mem_exn.get_word = get_word;
        })
  })


let backends : Backend.t String.Table.t =
  String.Table.create ()

let register_backend ~name backend =
  String.Table.add backends ~key:name ~data:backend

type img = Backend.Img.t
type mem = Mem.t
type mem_exn = Mem_exn.t

module T = struct
  type t = {
    name : string;
    mem  : mem;
    img  : img;
  }
end
include T

open Backend

let of_img img path =
  let addr_size, endian, sections =
    Img.(img.addr_size, img.endian, img.sections) in
  Mem.of_sections addr_size endian sections >>| fun mem -> {
    mem; img; name = path
  }

let of_backend backend data path =
  match String.Table.find backends backend with
  | None -> errorf "no such backend: '%s'" backend
  | Some loader -> match loader.of_data data with
    | None -> errorf "%s: failed to read file «%s»" backend path
    | Some img -> of_img img path

let autoload data path =
  let bs = String.Table.data backends in
  match List.filter_map bs ~f:(fun b -> b.of_data data) with
  | [] -> errorf "Autoloader: no suitable backend found"
  | [img] -> of_img img path
  | _ -> errorf "Autoloader: can't resolve proper backend"

let create_image path ?backend ~data =
  match backend with
  | None -> autoload data path
  | Some backend -> of_backend backend data path

let of_bigstring = create_image "rawdata"

let of_string ?backend ~data =
  of_bigstring ?backend ~data:(Bigstring.of_string data)

let mapfile path : Bigstring.t option =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  try
    let size = Unix.((fstat fd).st_size) in
    let data = Bigstring.map_file ~shared:false fd size in
    Unix.close fd;
    Some data
  with exn ->
    Unix.close fd;
    None

let readfile path : Bigstring.t =
  match mapfile path with
  | Some data -> data
  | None ->
    Bigstring.of_string (In_channel.read_all path)

let create ?backend ~path =
  try_with (fun () -> readfile path) >>= fun data ->
  match backend with
  | None -> autoload data path
  | Some backend -> of_backend backend data path

let filename t = t.name
let addr_size t = Img.addr_size t.img
let endian t = Img.endian t.img
let memory t = t.mem
let memory_exn t = memory_with_exn t.mem
let arch t = Img.arch t.img
let entry_point t = Img.entry t.img

let unsafe_hexdump (mem : mem_exn) : string =
  Mem_exn.hexdump mem Format.str_formatter ;
  Format.flush_str_formatter ()

let hexdump t : string = unsafe_hexdump (memory_exn t)


module Sec = struct
  type t = (T.t * int)
  let get pro (t,n) = pro (Img.sections t.img).(n)
  let name = get Section.name
  let addr = get Section.addr
  let size = get Section.size
  let perm = get Section.perm
  let is_readable sec = (perm sec).r
  let is_writable sec = (perm sec).w
  let is_executable sec = (perm sec).x
  let memory (t,n) = Mem.({ t.mem with secs = [|t.mem.secs.(n)|] })
  let memory_exn = Fn.compose memory_with_exn memory
  let hexdump t : string = unsafe_hexdump (memory_exn t)
end

type sec = Sec.t

let make_parts take t =
  Seq.init (Array.length @@ take t) ~f:(fun i -> (t,i))

let sections = make_parts (fun t -> Img.(t.img.sections))
let find_section t addr =
  Seq.find (sections t) ~f:(fun s -> Mem.contains (Sec.memory s) addr)

module Sym = struct
  type t = (T.t * int)
  let get pro (t,n) = pro (Img.symbols t.img).(n)
  let name = get Sym.name
  let addr = get Sym.addr
  let size = get Sym.size
  let is_function sym = match get Sym.kind sym with
    | `undef -> None
    | `func  -> Some true
  let value _ = unimplemented "Sym.value"

  let memory ((t,n) as s) =
    let addr = addr s in
    match find_section t addr with
    | None -> None
    | Some sec -> match size s with
      | None -> None
      | Some size ->
        let mem = Sec.memory sec in
        match Mem.view ~word_size:8 ~from:addr ~words:size mem with
        | Ok mem -> Some mem
        | Error _ -> None

  let memory_exn s = Option.(memory s >>| memory_with_exn)

  let hexdump t : string =
    Option.value_map (memory_exn t) ~default:"" ~f:unsafe_hexdump
end

type sym = Sym.t


let symbols = make_parts (fun t -> Img.(t.img.symbols))

let find_symbol t addr : sym option =
  Seq.find (symbols t) ~f:(fun sym -> Sym.addr sym = addr)


(* DRAGONS BE THERE! Stop here, a fellow wonderer  *)

module TestMem(M : ErrorMonad) = struct
  module Mem = MakeMem(M)
  open Mem
  open M

  let expect ?(print=false) ~errors ss =
    let width = 32 in
    let section (addr, off, size, data_size) = {
      Section.name = "test-section";
      Section.addr = Addr.of_int ~width addr;
      Section.perm = { r=true; w=true; x=true};
      Section.data = Bigstring.create data_size;
      Section.off;
      Section.size;
    } in
    let v = validate_sections @@
      Array.of_list @@
      List.map ss ~f:(section) in
    if print then begin
      match Validate.(result v) with
      | Ok () -> eprintf "No errors\n"
      | Error err -> eprintf "Errors: %s" @@ Error.to_string_hum err
    end;
    let has_errors = Validate.errors v <> [] in
    errors = has_errors

  TEST = expect ~errors:true  []
  TEST = expect ~errors:true  [0,0,0,0]
  TEST = expect ~errors:true  [0,0,0,1]
  TEST = expect ~errors:false [0,0,1,1]
  TEST = expect ~errors:true  [0,0,4,4; 2,0,4,4]
  TEST = expect ~errors:false [0,0,4,4; 4,0,4,4]
  TEST = expect ~errors:true  [0,0,4,4; 4,0,4,3]
  TEST = expect ~errors:true  [0,0,4,4; 4,1,4,4]
  TEST = expect ~errors:true  [0,2,4,4; 4,0,4,4]
  TEST = expect ~errors:true  [0,1,4,4; 4,0,4,4]
  TEST = expect ~errors:true  [0,0,4,4; 4,1,4,4]


  let find ss addr =
    let section addr = Backend.({
        Section.name = "test-section";
        Section.addr = Addr.of_int ~width:32 addr;
        Section.perm = { r=true; w=true; x=true};
        Section.data = Bigstring.create 4;
        Section.off  = 0;
        Section.size = 4;
      }) in
    of_sections Word_size.W32 LittleEndian @@
    Array.of_list (List.map ss ~f:section) >>= fun t ->
    find_region t Addr.(of_int ~width:32 addr) >>=
    fun (i,_) -> return i

  let get ~expect:n addr ss =
    find ss addr = return n

  (* we catch exceptions since null-monad throws them on error *)
  let none addr ss =
    try
      let r = find ss addr >>= fun _ -> return `Found in
      r <> return `Found
    with exn -> true

  TEST = get ~expect:0 0  [0]
  TEST = get ~expect:0 0  [0;4;8;12]
  TEST = get ~expect:0 1  [0;4;8;12]
  TEST = get ~expect:0 2  [0;4;8;12]
  TEST = get ~expect:0 3  [0;4;8;12]
  TEST = get ~expect:1 4  [0;4;8;12]
  TEST = get ~expect:1 5  [0;4;8;12]
  TEST = get ~expect:1 6  [0;4;8;12]
  TEST = get ~expect:1 7  [0;4;8;12]
  TEST = get ~expect:0 0  [0;8;12;16]
  TEST = get ~expect:0 1  [0;8;12;16]
  TEST = get ~expect:0 2  [0;8;12;16]
  TEST = get ~expect:0 3  [0;8;12;16]
  TEST = get ~expect:2 12 [0;8;12;16]
  TEST = get ~expect:2 13 [0;8;12;16]
  TEST = get ~expect:2 14 [0;8;12;16]
  TEST = get ~expect:2 15 [0;8;12;16]
  TEST = none 4   [0;8;12;16]
  TEST = none 5   [0;8;12;16]
  TEST = none 6   [0;8;12;16]
  TEST = none 7   [0;8;12;16]
  TEST = none 20  [0;8;12;16]
  TEST = none 21  [0;8;12;16]


  let width_of_size = function
    | Word_size.W32 -> 32
    | Word_size.W64 -> 64


  let create ?(word_size=Word_size.W32) ?(endian=LittleEndian) ss =
    let width = width_of_size word_size in
    let section (addr,off,data) = Backend.({
        Section.name = "test-section";
        Section.addr = Addr.of_int ~width addr;
        Section.perm = { r=true; w=true; x=true};
        Section.data = Bigstring.of_string data;
        Section.off  = off;
        Section.size = String.length data - off;
      }) in
    of_sections word_size endian @@
    Array.of_list (List.map ss ~f:section)


  module Input = struct
    let pos_ref = ref (Addr.of_int ~width:0 0)

    let read word_size endian input ss =
      create ~word_size ~endian ss >>= fun t -> input t ~pos_ref

    let check word_size endian cast_sign ~expect input ss =
      let expect = Int64.of_int expect in
      let r =
        read word_size endian input ss >>= fun word ->
        with_error (Word.to_int64 (cast_sign word)) in
      let test_ok = r = return expect in
      let (_ : unit m) =
        if not test_ok
        then
          r >>| eprintf "expected 0x%LX, got 0x%LX\n" expect
        else return () in
      test_ok

    module type Spec = sig
      val size: Word_size.t
      val big : bool
    end

    let make_check size big signed =
      check size (if big then BigEndian else LittleEndian) signed

    let assert_fail input ss =
      try
        let r =
          read Word_size.W32 LittleEndian input ss
          >>| fun (_ : word) -> `Not_failed in
        r <> return `Not_failed
      with exn -> true

    let zero size = Addr.of_int ~width:(width_of_size size) 0

    let signed = Word.signed
    let unsigned = ident

    open Word_size

    module Int8(S : Spec) = struct
      pos_ref := zero S.size
      let check = make_check S.size S.big signed
      TEST = check ~expect:(-1) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(-2) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(-3) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(-4) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
    end

    module UInt8(S : Spec) = struct
      pos_ref := zero S.size
      let check = make_check S.size S.big unsigned
      TEST = check ~expect:(0xFF) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(0xFE) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(0xFD) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
      TEST = check ~expect:(0xFC) Input.int8 [0,0, "\xFF\xFE\xFD\xFC"]
    end

    module Int16LE(Unit : Unit) = struct
      pos_ref := zero W32
      let check = make_check Word_size.W32 false unsigned
      TEST = check ~expect:(0x5E55) Input.uint16 [0,0, "\x55\x5E\x5D\x5C"]
      TEST = check ~expect:(0x5C5D) Input.uint16 [0,0, "\x55\x5E\x5D\x5C"]
    end

    module Int16BE(Unit : Unit) = struct
      pos_ref := zero W32
      let check = make_check Word_size.W32 true unsigned
      TEST = check ~expect:(0x555E) Input.uint16 [0,0, "\x55\x5E\x5D\x5C"]
      TEST = check ~expect:(0x5D5C) Input.uint16 [0,0, "\x55\x5E\x5D\x5C"]
    end

    module Int16LES(Unit : Unit) = struct
      pos_ref := Addr.(zero 32 ++ 2)
      let check = make_check Word_size.W32 false unsigned
      TEST = check ~expect:(0x5C5D) Input.uint16 [0,0, "\x55\x5E\x5D\x5C"]
    end

    let four_secs = [
      0x100AA,0, "\xAA\xAA\xAA\xAA";
      0x100BB,0, "\xAA\xAA\xAA\xAA";
      0x100CC,1, "\xAA\xAA\x5D\x5C";
      0x100DD,0, "\xAA\xAA\xAA\xAA";
      0x100EE,0, "\xAA\xAA\x5E\x5F";
    ]

    module Int16LEM(Unit : Unit) = struct
      pos_ref := Addr.(of_int ~width:32 0x100CD)
      let check = make_check Word_size.W32 false unsigned
      TEST = check ~expect:(0x5C5D) Input.uint16 four_secs
    end

    module IntSEG(Unit : Unit) = struct
      let addr = Addr.(of_int ~width:32 0x100CE)
      let () = pos_ref := addr
      let check = make_check Word_size.W32 false unsigned
      TEST = assert_fail Input.uint16 four_secs
      TEST = pos_ref.contents = addr
      TEST = check ~expect:0x5C Input.uint8 four_secs
    end

    module W32LE = struct let size = W32 let big = false end
    module W32BE = struct let size = W32 let big = true end
    module W64LE = struct let size = W64 let big = false end
    module W64BE = struct let size = W64 let big = true end

    TEST_MODULE = Int8(W32LE)
    TEST_MODULE = Int8(W32BE)
    TEST_MODULE = Int8(W64LE)
    TEST_MODULE = Int8(W64BE)
    TEST_MODULE = Int8(W32LE)
    TEST_MODULE = Int8(W32BE)
    TEST_MODULE = Int8(W64LE)
    TEST_MODULE = Int8(W64BE)
    TEST_MODULE = Int16LE(Unit)
    TEST_MODULE = Int16BE(Unit)
    TEST_MODULE = Int16LES(Unit)
    TEST_MODULE = Int16LEM(Unit)
    TEST_MODULE = IntSEG(Unit)
  end
  TEST_MODULE = Input

end

TEST_MODULE = TestMem(Or_error_with_validate)
TEST_MODULE = TestMem(ErrorNullMonad)
