open Core_kernel
open Bap.Std
open Bap_main

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out")

module Loader = struct
  type t = {
    libraries : image -> string seq;
    imports : image -> string seq;
  }
end


module Unit = struct
  type t = {
    name : string;
    imports : Set.M(String).t;
    libraries : Set.M(String).t;
  } [@@deriving bin_io, compare, sexp]


end

module Spec = struct
  open Image.Scheme

  let imports =
    Ogre.foreach Ogre.Query.(select (from external_reference))
      ~f:(fun (_, name) -> name)

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
    | None -> Seq.empty
    | Some strtab ->
      Seq.of_list (dynamic_contents img) |>
      Seq.filter_map ~f:(fun {tag; off} ->
          match Word.to_int tag, Word.to_int off with
          | Ok 1, Ok off -> Some (read ~strtab off)
          | _ -> None)

  let loader = {Loader.libraries; imports = Spec.(query imports)}
end

let print_plain =
  Seq.iter ~f:(Format.printf "%s@\n")

let () = Extension.Command.(begin
    declare "dependencies" (args $input)
      ~requires:["loader"]
  end) @@ fun input _ctxt ->
  match Image.create ~backend:"llvm" input with
  | Error err ->
    invalid_argf "failed to parse the file: %s"
      (Error.to_string_hum err) ()
  | Ok (img,_warns) ->
    print_plain @@ Elf.libraries img;
    Ok ()
