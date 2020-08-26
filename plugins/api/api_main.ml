open Core_kernel
open Result
open Bap.Std
include Self()

let try_with f = match try_with f with
  | Ok r -> Ok r
  | Error exn -> Error (`Fail exn)

let try_with_default ~default f = match try_with f with
  | Ok r -> r   | Error _ -> default

let getenv v =
  try Some (Sys.getenv v)
  with Caml.Not_found -> None

module Path = FilePath.DefaultPath.Abstract
type path = Path.filename

type api_descr = {
  lang : string;
  name : string;
} [@@deriving equal]

type api = {
  desc : api_descr;
  path : path;
}

type no_access = [ `No_access]

module Api_path = struct

  type t = path

  let (/) = Path.concat
  let to_string = Path.string_of_filename

  let expand_home path =
    Option.(value_map ~default:path ~f:ident
              (String.chop_prefix ~prefix:"~" (to_string path) >>= fun p ->
               getenv "HOME" >>= fun h ->
               Some Path.(filename_of_string h / filename_of_string p)))

  let of_string x = expand_home @@ Path.reduce @@ Path.filename_of_string x

  let basename = Path.basename
  let basename' x = to_string @@ basename @@ of_string x
  let is_apiable = FileUtil.(Or (Is_file, Is_link))

  let try_with_access f p =
    try Ok (f p)
    with _ -> Error `No_access

  let exists p = try_with_access FileUtil.(test Exists) (to_string p)

  let is_apiable_path p =
    let r = exists p >>= fun x ->
      try_with_access (FileUtil.test is_apiable) (to_string p) >>= fun y ->
      Ok (x && y) in
    match r with
    | Ok r -> r
    | _ -> false

  let is_dir p = try_with_access FileUtil.(test Is_dir) (to_string p)

  let find_files path =
    try_with_default ~default:[]
      (fun () -> FileUtil.find is_apiable (to_string path)
          (fun acc f -> of_string f :: acc) [])

  let path_of_desc desc = of_string desc.lang / of_string desc.name
  let with_lang path lang = path / of_string lang

  let lang_of_path path =
    to_string Path.(path |> reduce |> dirname |> basename)

  let name_of_path p = to_string @@ Path.basename p
  let desc_of_path p = {name = name_of_path p; lang = lang_of_path p;}

  let api_of_path path =
    if is_apiable_path path then
      Some {desc = desc_of_path path; path;}
    else None

  let is_api_at_path desc path = is_apiable_path @@ path / path_of_desc desc
  let find_all_paths paths desc = List.filter ~f:(is_api_at_path desc) paths
  let find_path paths desc = List.find ~f:(is_api_at_path desc) paths

  let find_apis ?lang path =
    let path' = match lang with
      | Some lang -> with_lang path lang
      | None -> path in
    find_files path' |>
    List.filter_map ~f:api_of_path

  let pp fmt p = Format.fprintf fmt "%s" (to_string p)

end

module Api = struct

  type t = api
  type op_err = [ no_access | `No_dst | `Fail of exn ]

  let lang t = t.desc.lang
  let name t = t.desc.name

  let create lang path =
    let path = Api_path.of_string path in
    if Api_path.is_apiable_path path then
      let name = Api_path.name_of_path path in
      Some {desc = {lang; name}; path;}
    else None

  let ok_if_true2 f x ~error = f x >>= fun r -> ok_if_true r ~error

  let cp api dest =
    ok_if_true2 Api_path.exists dest ~error:`No_dst >>= fun () ->
    ok_if_true2 Api_path.is_dir dest ~error:`No_dst >>= fun () ->
    let dest = Api_path.with_lang dest (lang api) in
    try_with
      (fun () ->
         let dest = Api_path.to_string dest in
         FileUtil.mkdir ~parent:true dest;
         FileUtil.cp [Api_path.to_string api.path] dest)

  let rm path desc =
    let path = Api_path.(path / path_of_desc desc) in
    ok_if_true2 Api_path.exists path ~error:`No_dst >>= fun () ->
    try_with (fun () -> FileUtil.rm [Api_path.to_string path])

end

module Api_error = struct

  let exit_with () = exit 1

  let api_exists path desc =
    exit_with @@ eprintf
      "Will not overwrite an API named %s at %s. Remove it before the updating.\n"
      desc.name (Api_path.to_string path)

  let api_not_exists ?path desc = match path with
    | Some path ->
      exit_with @@ eprintf "Can't find api %s:%s at %s\n"
        desc.lang desc.name (Api_path.to_string path)
    | None ->
      exit_with @@ eprintf "Can't find api %s:%s\n"
        desc.lang desc.name

  let no_path api =
    exit_with @@ eprintf
      "Can't store an api %s: permission denied\n" api.name

  let fail descr exn =
    exit_with @@ eprintf "%s: %s\n" descr (Exn.to_string exn)

  let sanity_fail api api'  =
    exit_with @@ eprintf
      "Sanitization error: there are conflicting specifications for the same API(s):\n\
       %s:%s at %s, %s\n\
       Each API should be specified only once. Please resolve the conflict manually,\n\
       either by removing the conflicting files or by renaming them.\n"
      api.desc.lang api.desc.name
      (Api_path.to_string api.path) (Api_path.to_string api'.path)
end

let add_api paths api =
  match Api_path.find_path paths api.desc with
  | Some path -> Api_error.api_exists path api.desc
  | None ->
    let rec add = function
      | [] -> Api_error.no_path api.desc
      | p :: paths' ->
        match Api.cp api p with
        | Ok () ->
          info "wrote %s:%s to %s"
            api.desc.lang api.desc.name (Api_path.to_string p)
        | Error `No_dst | Error `No_access ->
          info "Api.cp: skipping a non-writable destination %a" Api_path.pp p;
          add paths'
        | Error (`Fail exn) ->
          info "failed to write at %s path: %s"
            (Api_path.to_string p) (Exn.to_string exn);
          add paths' in
    add paths

let rem_api paths desc =
  let is_no_path ers =
    List.for_all ~f:(fun (er,_) -> match er with
        | Error `No_dst -> true
        | _ -> false) ers
    && List.length ers = List.length paths in
  let rm_api ers path = match Api.rm path desc with
    | Ok () ->
      info "removed api %s:%s from %s" desc.lang
        desc.name (Api_path.to_string path);
      ers
    | er -> (er, path) :: ers in
  let ers = List.fold ~f:rm_api ~init:[] paths in
  if is_no_path ers then Api_error.api_not_exists desc
  else
    List.iter ~f:(function
        | Error (`Fail exn), _ ->
          error "didn't removed api: %s" (Exn.to_string exn);
          Api_error.fail "can'remove api" exn
        | Error `No_access, p ->
          info "can't inspect path %s: permission denied"
            (Api_path.to_string p)
        |_ -> ()) ers

let add_files apis paths = List.iter ~f:(add_api paths) apis
let rem_files descrs paths = List.iter ~f:(rem_api paths) descrs

module Api_options = struct
  type t = {
    api_to_add : api list;
    api_to_rem : api_descr list;
    list_paths : bool;
    show_apis  : bool;
    api_paths  : string list;
  } [@@deriving fields]
end

let env_paths = match getenv "BAP_API_PATH" with
  | None -> []
  | Some p -> Path.path_of_string p

let all_paths o =
  env_paths @
  List.map ~f:Api_path.of_string
    (Api_config.api_path @ Api_options.api_paths o)

let get_file file = Some file

let files paths lang =
  List.(paths >>| Api_path.find_apis ~lang |> join >>| fun api ->
        Api_path.to_string api.path)

let mapper paths (module Api : Bap_api.S) =
  let files = files paths Api.language in
  Result.(Api.parse get_file files >>| Api.mapper)

let main paths proj =
  Bap_api.processors () |>
  List.map ~f:(mapper paths) |>
  Result.all |> function
  | Error e ->
    error "api wasn't applied: %a" Error.pp e;
    exit 1
  | Ok mappers ->
    Project.map_program proj ~f:(fun prog ->
        List.fold mappers ~init:prog ~f:(fun prog map -> map#run prog))

let list_of_paths paths =
  List.iter ~f:(fun p -> Format.printf "%s\n" (Api_path.to_string p)) paths

let api_of_paths paths =
  List.map ~f:Api_path.find_apis paths |> List.concat

let show_all_apis paths =
  let open Format in
  let pp_api api =
    printf "   %s : %s@." api.desc.name
      (Api_path.to_string api.path) in
  let print_lang apis = match apis with
    | [] -> ()
    | hd :: _ ->
      printf "@[<v>Language: %s@;" hd.desc.lang;
      List.iter ~f:pp_api apis;
      close_box ();
      print_newline () in
  let apis = api_of_paths paths |>
             List.sort ~compare:(fun x y -> String.compare x.desc.lang y.desc.lang) in
  List.iter ~f:print_lang (List.group ~break:(fun x y -> String.(x.desc.lang <> y.desc.lang)) apis);
  printf "Total number of available API: %d\n" (List.length apis)

let sanity_check paths =
  let is_same api api' =
    equal_api_descr api.desc api'.desc &&
    Path.compare api.path api'.path <> 0 in
  let check_for_pair api apis =
    match List.find ~f:(is_same api) apis with
    | None -> ()
    | Some api' -> Api_error.sanity_fail api api'  in
  let all = api_of_paths paths in
  List.iter ~f:(fun api -> check_for_pair api all) all

module Cmdline = struct
  let man = [
    `S "DESCRIPTION";
    `P "Automatically apply API bundled in the plugin. ";
    `S "SEE ALSO";
    `P "$(b,bap-api)(3), $(b,bap-plugin-frontc-parser)(1)"
  ]

  module Api_desc = struct
    type t = api_descr
    let empty = {lang="";name="";}
    let parser str = match String.split str ~on:':' with
      | [lang;name] -> `Ok {lang; name = Api_path.basename' name}
      | _ -> `Error "expected <lang>:<name>"
    let printer ppf t =
      Format.fprintf ppf "%s:%s" t.lang t.name
    let t = Config.converter parser printer empty
  end

  module Api = struct
    type t = api
    let parser str = match String.split str ~on:':' with
      | [lang;file] ->
        begin
          match Api.create lang file with
          | None -> `Error (sprintf "can't find api at %s" file)
          | Some api -> `Ok api
        end
      | _ -> `Error "expected <lang>:<file>"
    let printer ppf t =
      Format.fprintf ppf "%s:%s" t.desc.lang (Api_path.to_string t.path)
    let t = Config.converter
        parser printer {desc = Api_desc.empty ; path = Api_path.of_string ""}
  end

  let add_api : Api.t list Config.param =
    let doc = "Add specified api module(s) and exit. Each module
      should be of the form <lang>:<file>, where <lang> is the
      language in which API is written, and <file> is a path to
      the specification. Multiple modules can be added by specifying
      this option several times." in
    Config.(param ~deprecated:"will be removed in 2.0.0"
              (list Api.t) "add" ~doc)

  let remove_api : Api_desc.t list Config.param =
    let doc = "Removed specified api module and exit. Each module
      should be of the form <lang>:<file>, where <lang> is the
      language in which API is written, and <file> is a filename
      of api module in the $(b,api-list-paths) option. Multiple
      modules can be added by specifying this option several times." in
    Config.(param ~deprecated:"will be removed in 2.0.0"
              (list Api_desc.t) "remove" ~synonyms:["rem"] ~doc)

  let list_paths =
    let doc = "List of all registered paths" in
    Config.(flag "list-paths" ~doc)

  let show_apis =
    let doc = "Show all registered api" in
    Config.(flag "show" ~doc)

  let path =
    let doc = "Add a list of a paths where to store/search apis" in
    Config.(param (list dir) "path" ~doc ~default:[])

  let create a b c d e = Api_options.Fields.create a b c d e

  let dispatch_flags o =
    let open Api_options in
    match o.list_paths, o.show_apis with
    | false, false -> ()
    | show_paths, show_apis ->
      let paths = all_paths o in
      if show_paths then list_of_paths paths;
      if show_apis then show_all_apis paths;
      exit 0

  let dispatch_api_ops o =
    let open Api_options in
    match o.api_to_add, o.api_to_rem with
    | [],[] -> ()
    | _add,_rem ->
      let paths = all_paths o in
      add_files o.api_to_add paths;
      rem_files o.api_to_rem paths;
      exit 0

  let dispatch o =
    let open Api_options in
    let paths = all_paths o in
    sanity_check paths;
    dispatch_api_ops o;
    dispatch_flags o;
    Project.register_pass ~autorun:true ~deps:["abi"] (main paths)

  let normalize_paths ps =
    let norm p =
      if String.equal p "." then Sys.getcwd ()
      else p in
    List.map ~f:norm ps

  let () =
    Config.manpage man;
    Config.when_ready (fun {Config.get=(!)} ->
        let paths = normalize_paths !path in
        let o = create !add_api !remove_api
            !list_paths !show_apis paths in
        dispatch o)
end
