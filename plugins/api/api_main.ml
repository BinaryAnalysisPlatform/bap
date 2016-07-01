open Core_kernel.Std
open Bap_bundle.Std
open Bap.Std
include Self()

module type Api = Bap_api.S

let bundle = main_bundle ()

let in_language ~language name =
  match String.split ~on:'/' name with
  | ["api"; lang; _] -> lang = language
  | _ -> false

let (/) = Filename.concat

let get_file bundle lang filename =
  let name = Filename.temp_file "api" "lang" in
  let file = "api" / lang / filename in
  info "applying %s" file;
  match Bundle.get_file ~name bundle (Uri.of_string file) with
  | None -> None
  | Some uri -> Some (Uri.path uri)

let files bundle language  =
  Bundle.list bundle |> List.filter ~f:(in_language ~language) |>
  List.map ~f:Filename.basename

let mapper bundle (module Api : Api) =
  let get_file = get_file bundle Api.language in
  let files = files bundle Api.language in
  Result.(Api.parse get_file files >>| Api.mapper)

let main proj =
  Bap_api.processors () |>
  List.map ~f:(mapper bundle ) |>
  Result.all |> function
  | Error e ->
    error "api wasn't applied: %a" Error.pp e;
    proj
  | Ok mappers ->
    let prog = Project.program proj in
    List.fold mappers ~init:prog ~f:(fun prog map -> map#run prog) |>
    Project.with_program proj


type api = {lang : string; file : string}

let add_files api =
  let bundle = main_bundle () in
  List.map api ~f:(fun api ->
      Some ("api/" ^ api.lang ^ "/" ^ Filename.basename api.file),
      Uri.of_string api.file) |>
  Bundle.insert_files bundle

let rem_files api =
  main_bundle () |>
  Bundle.update ~f:(fun file -> match String.split file ~on:'/' with
      | ["api"; lang; file] ->
        if List.exists api ~f:(fun x ->
            x.lang = lang && x.file = Filename.basename file)
        then `Drop
        else `Copy
      | "api" :: _ ->
        warning "removing from the bundle a malformed entry: %s" file;
        `Drop
      | _ -> `Copy)


module Cmdline = struct
  let man = [
    `S "DESCRIPTION";
    `P "Automatically apply API bundled in the plugin. "
  ]

  module Api = struct
    type t = api
    let parser str = match String.split str ~on:':' with
      | [lang;file] -> `Ok {lang;file}
      | _ -> `Error "expected <lang>:<file>"
    let printer ppf t =
      Format.fprintf ppf "%s:%s" t.lang t.file
    let t = Config.converter parser printer {lang="";file=""}
  end

  let add_api : Api.t list Config.param =
    let doc = "Add specified api module(s) and exit. Each module
      should be of the form <lang>:<file>, where <lang> is the
      language in which API is written, and <file> is a path to
      the specification. Multiple modules can be added by specifying
      this option several times." in
    Config.(param (list Api.t) "add" ~doc)

  let remove_api : Api.t list Config.param =
    let doc = "Removed specified api module from the bundle and exit. The value
    format is the same, as in the $(b,api-add) option." in
    Config.(param (list Api.t) "remove" ~synonyms:["rem"] ~doc)

  let dispatch add rem = match add,rem with
    | [],[] -> Project.register_pass ~autorun:true ~deps:["abi"] main
    | add,rem -> add_files add; rem_files rem; exit 0

  let () =
    Config.manpage man;
    Config.when_ready (fun {Config.get=(!)} ->
        dispatch !add_api !remove_api)
end


let add_headers bundle file =
  let name = "api/" ^ Filename.basename file in
  try
    Bundle.insert_file ~name bundle (Uri.of_string file)
  with
  | Parsing.Parse_error ->
    printf "Could not add header file: parse error."; exit 1
