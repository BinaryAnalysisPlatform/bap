open Core_kernel.Std
open Or_error
open Bap.Std
open Ezjsonm

type response = Ezjsonm.t
type request = Ezjsonm.t
type target  = string with sexp_of
type uri = Uri.t
type links = Uri.t List1.t
type 'a resource = links * 'a
type id = string with sexp_of
type res_id = string with sexp_of
type res_ids = res_id list with sexp_of

type value = [
  | `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list
] with sexp_of


let minify = true


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


module Response = struct
  type t = response
  type msg = (id * value) list
  type loader = value
  type disassembler = value
  type transport = string
  type insn = value

  let to_string r = to_string ~minify r

  let create id (msg : msg) : t = `O ([
      "id", string id;
    ] @ msg)

  let error sev desc : msg = [
    "error", dict [
      "severity", string (string_of_severity sev);
      "description", string desc
    ]
  ]

  let capabilities ~version ts ls ds : msg = [
    "capabilities", dict [
      "version", string version;
      "loaders", `A ls;
      "disassemblers", `A ds;
      "transports", strings ts;
    ]
  ]

  let list_of_uris uris =
    List1.map uris ~f:Uri.to_string |>
    List1.to_list

  let disassembler
      ~name ~arch ~kinds ~has_name ~has_ops ~has_target
      ~has_bil : disassembler =
    dict [
      "name", string name;
      "architecture", string (Arch.to_string arch);
      "kinds", strings @@ Adt.strings_of_kinds kinds;
      "has_name", bool has_name;
      "has_ops", bool has_ops;
      "has_target", bool has_target;
      "has_bil", bool has_bil
    ]

  let string_of_sym s =
    Sexp.to_string (<:sexp_of<[`debug | `symtab]>> s)

  let strings_of_syms syms =
    List.intersperse ~sep:"," @@ List.map syms ~f:string_of_sym

  let loader ~name ~arch ~format syms : loader =
    dict [
      "name", string name;
      "architecture", string (Arch.to_string arch);
      "format", string format;
      "symbols", strings (strings_of_syms syms)
    ]

  let transport = ident


  let optional_field name json_of_value = function
    | None -> []
    | Some value -> [name, json_of_value value]

  let enum map x = strings (map x)

  let string_of_addr = Addr.string_of_value ~hex:false

  let memory_parameters m : msg = [
    "addr", string @@ string_of_addr @@ Memory.min_addr m;
    "size", string @@ Int.to_string  @@ Memory.length m;
  ]


  let resource links name props : msg = [
    name, dict @@ [
      "links", strings @@ list_of_uris links
    ] @ props
  ]

  let memory (links, m) : msg =
    resource links "memory" @@ memory_parameters m

  let bil_value = Fn.compose strings Adt.strings_of_bil

  let insn ?target ?bil mem insn : insn  =
    let module Insn = Disasm_expert.Basic.Insn in
    dict @@ [
      "name", string @@ Insn.name insn;
      "asm", string @@ Insn.asm insn;
      "kinds", strings @@ Adt.strings_of_kinds @@ Insn.kinds insn;
      "operands", strings @@ Adt.strings_of_ops
      @@ Array.to_list @@ Insn.ops insn;
    ] @ memory mem
      @ optional_field "target" string target
      @ optional_field "bil" bil_value bil

  let insns (insns : insn list) : msg = [
    "insns", `A insns;
  ]

  let list_of_perm sec =
    let (:=) v f = Option.some_if (f sec) v in
    List.filter_opt Image.Segment.([
        "r" := is_readable;
        "w" := is_writable;
        "x" := is_executable;
      ])


  let image ~secs (links,image) : msg =
    let open Image in
    let (/) = Fn.compose in
    resource links "image" @@
    List.map ~f:(fun (r,v) -> r, v image) [
      "arch", string / Arch.to_string / arch;
      "entry_point", string / string_of_addr / entry_point;
      "addr_size", string / Int.to_string / Size.to_bits / addr_size;
      "endian", string / Adt.string_of_endian / endian;
    ] @ optional_field "file" string (filename image) @ [
      "segments", strings secs;
    ]



  let symbol s mems : msg =
    let open Image.Symbol in [
      "symbol", dict @@ [
        "name", string @@ name s;
        "is_function", bool @@ is_function s;
        "is_debug", bool @@ is_debug s;
        "chunks", list (fun (links,m) -> dict @@ [
            "links", strings @@ list_of_uris links;
          ] @ memory_parameters m)
          (List1.to_list mems);
      ]
    ]

  let segment ~syms s mem : msg = [
    "segment", dict @@ [
      "name", string @@ Image.Segment.name s;
      "perm", strings @@ list_of_perm s;
      "symbols", strings syms;
    ] @ memory mem
  ]

  let resources name rs : msg = [name, strings rs]
  let segments = resources "segments"
  let symbols = resources "symbols"
  let images = resources "images"
  let chunks = resources "chunks"

  let added id : msg = ["resource", string id]

end


module Target = struct
  type t = target
  let arm insn ops : t = Adt.string_of_arm insn ops
end

module Request = struct
  type t = request with sexp_of
  let (/) = Fn.compose

  let of_string s =
    Or_error.try_with (fun () -> from_string s) |> function
    | Ok s -> Ok s
    | Error err -> errorf "Error: %s when parsing:\n%s\n"
                     (Error.to_string_hum err) s

  let pp_obj () obj =
    Sexp.to_string_hum @@ sexp_of_value obj

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



  let word arch =
    Word.of_int64 ~width:(Arch.addr_size arch |> Size.to_bits)

  let arch_of_string arch = match Arch.of_string arch with
    | Some arch -> arch
    | None -> invalid_argf "Unknown arch: %s" arch ()

  let value  = parse ident
  let string = parse get_string
  let string_opt = parse @@ Option.some / get_string
  let arch = parse @@ arch_of_string / get_string
  let addr arch = parse @@ word arch / Int64.of_string / get_string
  let url = parse @@ Uri.of_string / get_string

  let nulls constr =
    parse @@ ok_exn / all / get_list (constr / get_string)

  let kinds = nulls Adt.Parse.kind
  let preds = nulls Adt.Parse.pred

  let accept_load_file f obj =
    url obj ["url"] >>= fun uri ->
    if mem obj ["loader"]
    then string obj ["loader"] >>= fun loader ->
      return (f ?loader:(Some loader) uri)
    else return (f ?loader:None uri)

  let optional get obj name ~default  =
    if mem obj [name] then get obj [name] else return default

  let accept_load_chunk f obj =
    url obj       ["url"]  >>= fun url ->
    arch obj      ["arch"] >>= fun arch ->
    addr arch obj ["addr"] >>= fun addr ->
    return (f addr arch url)


  let accept_get_insns f obj =
    string obj ["resource"] >>= fun id ->
    optional preds obj "stop_conditions" ~default:[] >>= fun ks ->
    optional string_opt obj "backend" ~default:None >>= fun backend ->
    return (f ?backend ks id)

  let accept_init f obj = string obj ["version"] >>| f

  let accept_get_resource f obj =
    try_with (fun () -> get_string obj) >>| f

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
      "load_file"         >> load_file    ||
      "load_memory_chunk" >> load_chunk   ||
      "get_insns"         >> get_insns    ||
      "get_resource"      >> get_resource in
    match chain with
    | Some r -> r
    | None -> errorf "One of the required properties is not found: %a"
                pp_obj obj

  let id obj =
    string (Ezjsonm.value obj) ["id"]

end
