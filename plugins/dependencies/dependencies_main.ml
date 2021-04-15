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
      name; libraries;
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

  let search paths file =
    if Sys.file_exists file then Some file
    else List.find_map paths ~f:(fun folder ->
        let path = Filename.concat folder file in
        Option.some_if (Sys.file_exists path) path)

  let of_image name =
    match Image.create ~backend:"llvm" name with
    | Error err ->
      warning "failed to load the file %S: %a@\n"
        name Error.pp err;
      empty name
    | Ok (img,_) -> Spec.query name img

  let load ~context paths name =
    match search paths name with
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

  let load ~recursive ~context paths root =
    let load = Unit.load ~context paths in
    let unit = load root in
    let init = {
      root;
      units = Map.singleton (module String) root unit;
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

let recursive = Extension.Command.flag "recursive"
    ~aliases:["r"]

let formats = Extension.Type.enum [
    "yaml", State.pp_yaml;
    "sexp", State.pp_sexp;
    "json", State.pp_json;
    "graph", State.pp_graph;
  ]

let format = Extension.Command.parameter formats "format"
    ~aliases:["o"]

let () = Extension.Command.(begin
    declare "dependencies" (args $format $recursive $paths $input)
      ~requires:["loader"; "cache"]
  end) @@ fun pp recursive paths input ctxt ->
  let context = Extension.Configuration.digest ctxt in
  let r = State.load ~recursive ~context (List.concat paths) input in
  Format.printf "%a@." pp r;
  Ok ()
