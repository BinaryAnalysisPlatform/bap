let doc = "
# DESCRIPTION

Outputs program dependencies such as libraries and symbols. The
information is collected recursively with various output options,
including dependency graph, YAML, JSON, and SEXP.

The information includes the list of imported libraries as well as a
set of imported and exported symbols. The information could be
collected recursively, when the $(b,--recursive) option is
specified. In a recursive mode, the list of paths where to search for
libraries could be specified with the $(b,--library-path) option that
accepts a list of paths. It is also possible to use the host ldconfig
cache (or specify custom library config) via the $(b,ldconfig)
parameter. Information about each individual dependency is cached, so
consecutive calls to bap will reuse the available information and
terminate quickly.

# EXAMPLES

The default format YAML, which suits best for human consumption. The
tool is designed to be used together with YAML and JSON query tools,
suchs as $(b,yq) and $(b,jq), e.g.,

- Getting the list of imported libraries

```
bap dependencies /bin/ls | yq e '.ls.libraries' -

```

- Counting the number of imported symbols

```
bap dependencies /bin/ls | yq e '.ls.imports | length' -
```

- Generating the dependency graph

```
bap dependency /bin/ls -ograph
```

- Running recursively using the ldconfig cache

```
bap dependency /bin/ls --recursive --ldconfig
```

- Specifying custom ldconfig with a custom root folder (for mounted images)

```
bap dependency /bin/ls --root=/mnt/image --ldconfig='cat ld.so.cache'
```
"

open Core_kernel
open Bap.Std
open Regular.Std
open Bap_main

include Loggers()


type info = {
  name : string;
  imports : Set.M(String).t;
  exports : Set.M(String).t;
  libraries : string list;
} [@@deriving bin_io, compare, sexp]


module Ldconfig : sig
  type cache
  val load : string -> cache
  val lookup : cache -> string -> string option
end = struct
  type cache = string Map.M(String).t

  let parse_line s =
    match String.split (String.strip s) ~on:'>' with
    | [] | [_] | _ :: _ :: _ :: _ -> None
    | [_;s] -> Some (String.strip s)

  let load ldconfig =
    let ldconfig = UnixLabels.open_process_in ldconfig in
    protect ~f:(fun () ->
        In_channel.fold_lines ldconfig ~f:(fun cache s ->
            match parse_line s with
            | None -> cache
            | Some path ->
              Map.set cache (Filename.basename path) path)
          ~init:String.Map.empty)
      ~finally:(fun () ->
          match UnixLabels.close_process_in ldconfig with
          | WEXITED 0 -> ()
          | WEXITED n ->
            warning "ldconfig terminated with a non-zero code: %d" n
          | WSIGNALED _ | WSTOPPED _->
            warning "ldconfig was killed or suspended by a signal")

  let lookup = Map.find
end

module Spec = struct
  open Image.Scheme
  open Ogre.Syntax

  let imports = Ogre.foreach Ogre.Query.(begin
      select (from external_reference $ named_symbol)
        ~join:[[field name]]
        ~where:(named_symbol.(addr) = int 0L)
    end) ~f:(fun (_, name) _ -> name)

  let exports = Ogre.foreach Ogre.Query.(begin
      select (from named_symbol)
        ~where:(named_symbol.(addr) <> int 0L)
    end) ~f:(fun (_, name) -> name)


  let libraries = Ogre.collect Ogre.Query.(select (from require))

  let set_of_seq =
    Seq.fold ~f:Set.add ~init:(Set.empty (module String))

  let build name =
    imports >>| set_of_seq >>= fun imports ->
    exports >>| set_of_seq >>= fun exports ->
    libraries >>| Seq.to_list >>| fun libraries -> {
      name = Filename.basename name;
      libraries;
      imports;
      exports;
    }

  let query name image =
    match Ogre.eval (build name) (Image.spec image) with
    | Error err ->
      failwithf "Query failed: %s" (Error.to_string_hum err) ()
    | Ok r -> r
end

module Unit = struct
  type t = info [@deriving bin_io, compare, sexp]

  let empty name = {
    name;
    imports = Set.empty (module String);
    exports = Set.empty (module String);
    libraries = [];
  }

  module Io = struct type t = info [@@deriving bin_io] end

  let reader = Data.Read.create ()
      ~of_bigstring:(Binable.of_bigstring (module Io))

  let writer = Data.Write.create ()
      ~to_bigstring:(fun data -> Binable.to_bigstring (module Io) data)


  let search_cache cache name = match cache with
    | None -> None
    | Some cache -> Ldconfig.lookup cache name

  let reroot ?root path = match root with
    | Some root when Filename.is_absolute path ->
      Filename.concat root path
    | _ -> path

  let search ?root ?ldconfig paths file =
    let file = reroot ?root file in
    if Sys.file_exists file then Some file
    else if Filename.is_absolute file then None
    else match search_cache ldconfig file with
      | Some path -> Some path
      | None -> List.find_map paths ~f:(fun folder ->
          let path = Filename.concat folder file in
          Option.some_if (Sys.file_exists path) path)

  let of_image name =
    match Image.create ~backend:"llvm" name with
    | Error err ->
      warning "failed to load the file %S: %a@\n"
        name Error.pp err;
      empty name
    | Ok (img,_) -> Spec.query name img

  let load ?root ?ldconfig ~context paths name =
    match search ?root ?ldconfig paths name with
    | None ->
      warning "missing dependency: %s" name;
      empty name
    | Some name ->
      let empty =
        Data.Cache.Digest.create ~namespace:"dependencies" in
      let digest = Data.Cache.Digest.add empty "%s%s" context name in
      let cache = Data.Cache.Service.request reader writer in
      match Data.Cache.load cache digest with
      | None ->
        let data = of_image name in
        Data.Cache.save cache digest data;
        info "caching %s as %a" name Data.Cache.Digest.pp digest;
        data
      | Some data ->
        info "%s is cached" name;
        data

  let print_set ~pp_nil ~pp_sep ~pp_elt ppf elts =
    match Set.max_elt elts with
    | None -> pp_nil ppf ()
    | Some last ->
      Set.iter elts ~f:(fun elt ->
          pp_elt ppf elt;
          if not (String.equal elt last)
          then pp_sep ppf ())

  module Sexp = struct
    let pp_list ppf elts =
      Format.pp_print_list Format.pp_print_string ppf elts
        ~pp_sep:Format.pp_print_space

    let pp_set = print_set
        ~pp_nil:Format.pp_print_space
        ~pp_sep:Format.pp_print_space
        ~pp_elt:Format.pp_print_string

    let pp_list_field ppf = function
      | (_,[]) -> ()
      | (name,elts) ->
        Format.fprintf ppf "@ @[<hv2>(%s@ %a)@]"
          name pp_list elts

    let pp_set_field ppf (name,elts) =
      if not (Set.is_empty elts) then
        Format.fprintf ppf "@ @[<hv2>(%s@ %a)@]"
          name pp_set elts

    let pp ppf {name; imports; exports; libraries} =
      Format.fprintf ppf "@[<hv2>(%s%a%a%a)@]" name
        pp_set_field ("imports",imports)
        pp_set_field ("exports",exports)
        pp_list_field ("libraries",libraries)
  end

  module Deps = struct
    let pp ppf {name; libraries} =
      List.iter libraries ~f:(fun dep ->
          Format.fprintf ppf "%S -> %S@\n" name dep)
  end

  module Json = struct
    let pp_elt ppf = Format.fprintf ppf "%S"
    let pp_sep ppf () = Format.fprintf ppf ",@ "
    let pp_nil _ () = ()
    let pp_set = print_set ~pp_nil ~pp_sep ~pp_elt
    let pp_list = Format.pp_print_list pp_elt ~pp_sep
    let pp ppf {name; imports; exports; libraries} =
      Format.fprintf ppf {|@[<hv2>
  %S: {
    "imports": [@,%a],
    "exports": [@,%a],
    "libraries": [@,%a]
  }
@]|}
        name pp_set imports pp_set exports pp_list libraries
  end

  module Yaml = struct
    let pp_elt ppf = Format.fprintf ppf "    - %s"
    let pp_sep = Format.pp_print_newline
    let pp_nil ppf () = Format.pp_print_string ppf ""
    let pp_set = print_set ~pp_nil ~pp_elt ~pp_sep
    let pp_elts = Format.pp_print_list ~pp_sep pp_elt
    let pp_list ppf xs = match xs with
      | [] -> pp_nil ppf ()
      | xs -> pp_elts ppf xs

    let pp ppf {name; imports; exports; libraries} =
      Format.fprintf ppf {|
%s:
  imports:
%a
  exports:
%a
  libraries:
%a
|} name pp_set imports pp_set exports pp_list libraries
  end

end

module State = struct
  type t = {
    root : string;
    units : Unit.t Map.M(String).t;
  }

  let load ?root ?ldconfig ~recursive ~context paths path =
    let ldconfig = Option.map ldconfig ~f:Ldconfig.load in
    let load = Unit.load ?root ?ldconfig ~context paths in
    let unit = load path in
    let path = Filename.basename path in
    let init = {
      root=path;
      units = Map.singleton (module String) path unit;
    } in
    let rec closure state name =
      if Map.mem state.units name then state
      else
        let unit = load name in
        let state = {
          state with units = Map.add_exn state.units name unit
        } in
        load_deps state unit
    and load_deps state unit =
      List.fold unit.libraries ~init:state ~f:closure in
    if recursive
    then load_deps init unit
    else init

  let print_units ?(pp_sep=Format.pp_print_cut) pp_unit ppf {units} =
    match Map.max_elt units with
    | None -> ()
    | Some (last,_) ->
      Map.iteri units ~f:(fun ~key:name ~data:unit ->
          Format.fprintf ppf "%a@\n" pp_unit unit;
          if not (String.equal name last) then pp_sep ppf ())

  let pp_sexp = print_units Unit.Sexp.pp

  let pp_graph ppf g =
    Format.fprintf ppf "@[digraph %S {@\n%a}" g.root
      (print_units Unit.Deps.pp) g

  let pp_comma ppf () = Format.fprintf ppf ",@\n"

  let pp_json ppf g =
    Format.fprintf ppf "@[<hv2>{%a}@]"
      (print_units ~pp_sep:pp_comma Unit.Json.pp) g

  let pp_yaml = print_units Unit.Yaml.pp
end

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out")

let paths = Extension.Command.parameters Extension.Type.(list string)
    "library-paths"
    ~doc:"Specify a list of directories where to search for libraries."

let recursive = Extension.Command.flag "recursive"
    ~aliases:["r"]
    ~doc:"Calls the commmand recursively on the obtained dependencies. "

let ldconfig = Extension.Command.parameter
    Extension.Type.(some string) "ldconfig"
    ~as_flag:(Some "ldconfig -p")
    ~doc:"A command that returns an ldconfig-like cache. \
          If the option is specified without an argument then a \
          system ldconfig is used (with the $(b,-p) option) to \
          obtain the library cache. If an argument is passed \
          it should be a command that returns a list of entries, \
          separated with $(b,=>), with the right entry being a \
          path to a library. (See $(b,ldconfig -p) for an example."

let root = Extension.Command.parameter
    Extension.Type.(some string) "root"
    ~doc: "Specify the default root directory. \
           All absolute paths (including those that are specified \
           on the command line) are prefixed with the specified root."

let formats = Extension.Type.enum [
    "yaml", `Yaml;
    "sexp", `Sexp;
    "json", `Json;
    "graph", `Graph;
  ]

let pp = function
  | `Yaml -> State.pp_yaml
  | `Sexp -> State.pp_sexp
  | `Json -> State.pp_json
  | `Graph -> State.pp_graph

let format = Extension.Command.parameter formats "format"
    ~aliases:["o"]

let () = Extension.Command.(begin
    declare "dependencies" ~doc
      (args $root $ldconfig $format $recursive $paths $input)
      ~requires:["loader"; "cache"]
  end) @@ fun root ldconfig fmt  recursive paths input ctxt ->
  let context = Extension.Configuration.digest ctxt in
  let paths = List.concat paths in
  let r = State.load ?root ?ldconfig ~recursive ~context paths input in
  Format.printf "%a@." (pp fmt) r;
  Ok ()
