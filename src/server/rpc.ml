open Core_kernel.Std
open Or_error
open Bap.Std
open Ezjsonm

type response = Ezjsonm.t
type request = Ezjsonm.t
type target  = string with sexp_of
type resource = string with sexp_of
type uri = Uri.t
type id = string with sexp_of

let sexp_of_response = Fn.compose Ezjsonm.to_sexp Ezjsonm.value
let sexp_of_request = sexp_of_response

module Id = String

type severity = [
  | `Critical
  | `Error
  | `Warning
] with bin_io, compare, sexp

let string_of_severity s =
  Sexp.to_string @@ sexp_of_severity s

let obj name o id = `O [
    id,  string id;
    name, `O o
  ]

module Response = struct
  type t = response
  type msg = id -> t
  type insn = value

  let create id (msg : msg) : t = msg id

  let error id sev desc : msg =
    obj "error" [
      "severity", string (string_of_severity sev);
      "description", string desc
    ]

  let capabilities : msg =
    obj "capabilities" [

    ]

  let list_of_uris uris =
    List1.map uris ~f:Uri.to_string |>
    List1.to_list

  let resource ~id links name props : msg = obj name @@ [
      "id", string id;
      "links", strings @@ list_of_uris links
    ] @ props

  let disassembler
      ~name ~arch ~kinds ~has_name ~has_ops ~has_target
      ~has_bil : msg =
    obj "disassembler" [
      "name", string name;
      "architecture", string arch;
      "kinds", strings @@ Adt.strings_of_kinds kinds;
      "has-name", bool has_name;
      "has-ops", bool has_ops;
      "has-target", bool has_target;
      "has-bil", bool has_bil
    ]


  let string_of_sym s =
    Sexp.to_string (<:sexp_of<[`debug | `symtab]>> s)

  let strings_of_syms syms =
    List.intersperse ~sep:"," @@ List.map syms ~f:string_of_sym

  let loader ~name ~arch ~format syms : msg =
    obj "loader" [
      "name", string name;
      "architecture", string arch;
      "format", string format;
      "symbols", strings (strings_of_syms syms)
    ]

  let optional_field name json_of_value = function
    | None -> []
    | Some value -> [name, json_of_value value]

  let enum map x = strings (map x)


  let bil_value = Fn.compose strings Adt.strings_of_bil

  let insn ?target ?bil ~mem_id insn : insn  =
    let open Disasm.Basic in dict @@ [
      "memory", string @@ mem_id;
      "name", string @@ Insn.name insn;
      "asm", string @@ Insn.asm insn;
      "kinds", strings @@ Adt.strings_of_kinds @@ Insn.kinds insn;
      "operands", strings @@ Adt.strings_of_ops
      @@ Array.to_list @@ Insn.ops insn;
    ] @ optional_field "target" string target
      @ optional_field "bil" bil_value bil

  let insns (insns : insn list) : msg = fun id -> `O [
      "id", string id;
      "insns", `A insns;
    ]

  let list_of_perm sec =
    let (:=) v f = Option.some_if (f sec) v in
    List.filter_opt Section.([
        "r" := is_readable;
        "w" := is_writable;
        "x" := is_executable;
      ])

  let section ~img ~sec ~mem links s : msg =
    resource ~id:sec links "section" [
      "name", string @@ Section.name s;
      "image",  string img;
      "memory", string mem;
      "perm", strings @@ list_of_perm s
    ]


  let string_of_addr = Addr.string_of_value ~hex:false

  let image ~img ~secs ~syms links image : msg =
    let open Image in
    let ids = [
      "sections", strings secs;
      "symbols",  strings syms;
    ] in
    let (/) = Fn.compose in
    resource ~id:img links "image" @@
    List.map ~f:(fun (r,v) -> r, v image) [
      "arch", string / Arch.to_string / arch;
      "entry-point", string / string_of_addr / entry_point;
      "addr-size", string / Int.to_string / Size.to_bits / addr_size;
      "endian", string / Adt.string_of_endian / endian;
    ] @ optional_field "file" string (filename image) @ ids

  let memory ?sec ?sym ~mem links m : msg =
    resource ~id:mem links "memory" @@ [
      "addr", string @@ Int.to_string  @@ Memory.size m;
      "size", string @@ string_of_addr @@ Memory.min_addr m;
    ] @ optional_field "section" string sec
      @ optional_field "symbol"  string sym

  let symbol ~sec ~sym ~mem links s : msg =
    let open Symbol in
    resource ~id:sym links "symbol" @@ [
      "name", string @@ name s;
      "is_function", bool @@ is_function s;
      "is_debug", bool @@ is_debug s;
      "section", string sec;
      "memory", strings mem;
    ]

  let resources name rs : msg = fun id ->
    `O [
      "id", string  id;
      name, strings rs;
    ]
  let sections = resources "sections"
  let symbols = resources "symbols"
  let images = resources "images"
  let chunks = resources "chunks"
end


module Target = struct
  type t = target
  let arm insn ops : t = Adt.string_of_arm insn ops
end

module Request = struct
  type t = request with sexp_of
  let (/) = Fn.compose

  let pp_obj () v =
    Sexp.to_string_hum @@ to_sexp v

  let pp_path () path =
    String.concat ~sep:"." path

  let no_value path v =
    errorf "Path '%a' not found in object %a" pp_path path pp_obj v
  let protocol path msg v =
    errorf "Failed to parse path %a : %s. Object: %a"
      pp_path path msg pp_obj v


  let parse pro v path =
    try Ok (find v path |> pro) with
    | Not_found -> no_value path v
    | Parse_error (v,msg) -> protocol path msg v
    | exn -> protocol path Exn.(to_string exn) v


  let value  = parse ident
  let string = parse get_string
  let arch = parse @@ Arch.of_string / get_string
  let addr = parse @@ ok_exn / Adt.Parse.word / get_string
  let endian = parse @@ ok_exn / Adt.Parse.endian / get_string
  let url = parse @@ Uri.of_string / get_string
  let kinds =
    parse @@ ok_exn / all / get_list (Adt.Parse.kind / get_string)

  let accept_load_file f obj =
    url obj ["url"] >>= fun uri ->
    if mem obj ["loader"]
    then string obj ["loader"] >>= fun loader ->
      f ?loader:(Some loader) uri
    else f ?loader:None uri

  let accept_load_chunk f obj =
    url obj    ["url"]     >>= fun url ->
    addr obj   ["address"] >>= fun addr ->
    arch obj   ["arch"]    >>= fun arch ->
    endian obj ["endian"]  >>= fun endian ->
    f addr arch endian url

  let accept_get_insns f obj =
    string obj ["resource"] >>= fun id ->
    if mem obj ["stop-conditions"]
    then kinds obj ["stop-conditions"] >>= fun kinds ->
      f kinds id
    else f [] id

  let accept_init f obj = string obj ["version"] >>= f

  let accept_get_resource f obj =
    try_with (fun () -> get_string obj) >>= f

  let accept obj
      ~init ~load_file ~load_chunk ~get_insns ~get_resource =
    let obj = Ezjsonm.value obj in
    let init = accept_init init in
    let load_file = accept_load_file load_file in
    let load_chunk = accept_load_chunk load_chunk in
    let get_insns = accept_get_insns get_insns in
    let get_resource = accept_get_resource get_resource in
    let (>>) path fn = if mem obj [path]
      then Some (value obj [path] >>= fn)
      else None in
    let (||) = Option.merge ~f:(fun x y -> x) in
    let chain =
      "init"              >> init         ||
      "load-file"         >> load_file    ||
      "load-memory-chunk" >> load_chunk   ||
      "get-insns"         >> get_insns    ||
      "get-resource"      >> get_resource in
    match chain with
    | Some r -> r
    | None -> errorf "One of the required properties is not found: %a"
                pp_obj obj


  let id obj =
    string (Ezjsonm.value obj) ["id"]

end
