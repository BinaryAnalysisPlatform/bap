open Core_kernel
open Bap.Std
open Regular.Std
open Bap_main

include Loggers()

module Loader = struct
  type t = {
    libraries : image -> string seq;
    imports : image -> string seq;
    exports : image -> string seq;
  }
end

module Unit = struct
  type t = {
    name : string;
    imports : string list;
    exports : string list;
    libraries : string list;
  } [@@deriving bin_io, compare, sexp]

  let empty name = {
    name;
    imports = [];
    exports = [];
    libraries = [];
  }

  module Io = struct type nonrec t = t [@@deriving bin_io] end

  let reader = Data.Read.create ()
      ~of_bigstring:(Binable.of_bigstring (module Io))

  let writer = Data.Write.create ()
      ~to_bigstring:(fun data -> Binable.to_bigstring (module Io) data)

  let search paths file =
    if Sys.file_exists file then Some file
    else List.find_map paths ~f:(fun folder ->
        let path = Filename.concat folder file in
        Option.some_if (Sys.file_exists path) path)

  let of_image {Loader.libraries; imports; exports} name =
    match Image.create ~backend:"llvm" name with
    | Error err ->
      warning "failed to load the file %S: %a@\n"
        name Error.pp err;
      empty name
    | Ok (img,_) -> {
        name;
        libraries = Seq.to_list (libraries img);
        imports = Seq.to_list (imports img);
        exports = Seq.to_list (exports img);
      }

  let load ~context loader paths name =
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
        let data = of_image loader name in
        Data.Cache.save cache digest data;
        info "caching %s as %a" name Data.Cache.Digest.pp digest;
        data
      | Some data ->
        info "%s is cached" name;
        data


  let pp_elts ppf elts =
    Format.pp_print_list Format.pp_print_string ppf elts
      ~pp_sep:Format.pp_print_space

  let pp_field ppf = function
    | (_,[]) -> ()
    | (name,elts) ->
      Format.fprintf ppf "@ @[<hv2>(%s@ %a)@]"
        name pp_elts elts

  let pp ppf {name; imports; exports; libraries} =
    Format.fprintf ppf "@[<hv2>(%s%a%a%a)@]" name
      pp_field ("imports",imports)
      pp_field ("exports",exports)
      pp_field ("libraries",libraries)
end

module State = struct
  type t = {
    root : string;
    units : Unit.t Map.M(String).t;
  }

  let load ~recursive ~context paths loader root =
    let load = Unit.load ~context loader paths in
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

  let pp ppf {units} =
    Map.iter units ~f:(fun unit ->
        Format.fprintf ppf "%a@\n" Unit.pp unit)

end

module Spec = struct
  open Image.Scheme

  let imports = Ogre.foreach Ogre.Query.(begin
      select (from external_reference $ named_symbol)
        ~join:[[field name]]
        ~where:(named_symbol.(addr) = int 0L)
    end) ~f:(fun (_, name) _ -> name)

  let exports = Ogre.foreach Ogre.Query.(begin
      select (from named_symbol)
        ~where:(named_symbol.(addr) <> int 0L)
    end) ~f:(fun (_, name) -> name)

  let query what image =
    match Ogre.eval what (Image.spec image) with
    | Error err ->
      failwithf "Query failed: %s" (Error.to_string_hum err) ()
    | Ok r -> r
end

module Elf = struct

  type dyn = {
    tag : word;
    off : word;
  }

  let section_by_name img name =
    Image.memory img |>
    Memmap.to_sequence |>
    Seq.find_map ~f:(fun (mem,tag) ->
        match Value.get Image.section tag with
        | Some n when String.equal n name -> Some mem
        | _ -> None)

  let dynamic_contents img =
    match section_by_name img ".dynamic" with
    | None -> []
    | Some mem ->
      let word_size = (Image.addr_size img :> Size.t) in
      Memory.fold ~word_size mem ~f:(fun data (acc,dyn) ->
          match dyn with
          | None -> acc,Some data
          | Some tag -> {tag;off=data}::acc,None)
        ~init:([],None) |> function
      | acc,_ -> List.rev acc

  let read ~strtab pos =
    let strtab = Memory.to_buffer strtab in
    let rec loop off =
      if Char.equal (Bigsubstring.get strtab off) '\x00'
      then Bigsubstring.sub strtab ~pos ~len:(off-pos)
      else loop (off+1) in
    Bigsubstring.to_string @@ loop pos

  let libraries img =
    match section_by_name img ".dynstr" with
    | None ->
      warning "not a dynamic object";
      Seq.empty
    | Some strtab ->
      Seq.of_list (dynamic_contents img) |>
      Seq.filter_map ~f:(fun {tag; off} ->
          match Word.to_int tag, Word.to_int off with
          | Ok 1, Ok off -> Some (read ~strtab off)
          | _ -> None)

  let loader = {
    Loader.libraries;
    imports = Spec.(query imports);
    exports = Spec.(query exports);
  }
end


let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out")

let paths = Extension.Command.parameters Extension.Type.(list string)
    "library-paths"

let recursive = Extension.Command.flag "recursive"
    ~aliases:["r"]

let () = Extension.Command.(begin
    declare "dependencies" (args $recursive $paths $input)
      ~requires:["loader"; "cache"]
  end) @@ fun recursive paths input ctxt ->
  let context = Extension.Configuration.digest ctxt in
  let r = State.load ~recursive ~context (List.concat paths) Elf.loader input in
  Format.printf "%a@." State.pp r;
  Ok ()
